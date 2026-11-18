module SafetyFirst.Specs.SeqSpec

open NUnit.Framework
open FsCheck
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Numbers

let errorsAndThrowsOrNeither safeVersion unsafeVersion = 
  let throws = 
    try 
      match box (Lazy.force unsafeVersion) with
      | :? (obj seq) as xs -> xs |> Seq.toList |> ignore
      | _ -> ()
      false
    with
    | _ -> true

  let isError = 
    match safeVersion with
    | Error _ -> true
    | Ok _ -> false

  in (throws && isError) || (not throws && not isError)

let errorsWheneverThrows1 safeVersion unsafeVersion = 
  let prop inputs  =
    errorsAndThrowsOrNeither (safeVersion inputs) (lazy (unsafeVersion inputs))

  Check.QuickThrowOnFailure prop

let errorsWheneverThrows2 safeVersion unsafeVersion = 
  let prop input1 input2 =
    errorsAndThrowsOrNeither (safeVersion input1 input2) (lazy (unsafeVersion input1 input2))

  Check.QuickThrowOnFailure prop

let errorsWheneverThrows3 safeVersion unsafeVersion = 
  let prop input1 input2 input3 =
    errorsAndThrowsOrNeither (safeVersion input1 input2 input3) (lazy (unsafeVersion input1 input2 input3))

  Check.QuickThrowOnFailure prop

let errorsWheneverThrows4 safeVersion unsafeVersion = 
  let prop input1 input2 input3 input4 =
    errorsAndThrowsOrNeither (safeVersion input1 input2 input3 input4) (lazy (unsafeVersion input1 input2 input3 input4))

  Check.QuickThrowOnFailure prop

let errorsWheneverThrowsForSeq1 safeVersion unsafeVersion = 
  errorsWheneverThrows1 (List.toSeq >> safeVersion) (List.toSeq >> unsafeVersion)

let errorsWheneverThrowsForSeq2 safeVersion unsafeVersion = 
  errorsWheneverThrows2 
    (fun a xs -> safeVersion a (List.toSeq xs))
    (fun a xs -> unsafeVersion a (List.toSeq xs))

let averageFloats' (xs:float seq) = Seq.average' xs
let averageFloats (xs:float seq) = Seq.average xs
let averageByFloats' (projection:_ -> float) xs = Seq.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = Seq.averageBy projection xs


[<Test>]
let ``Safe Seq functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrowsForSeq1 averageFloats'        averageFloats
  errorsWheneverThrowsForSeq2 averageByFloats'      averageByFloats
  errorsWheneverThrowsForSeq2 Seq.chunkBySize'      Seq.chunkBySize
  errorsWheneverThrowsForSeq1 Seq.exactlyOne'       Seq.exactlyOne
  errorsWheneverThrowsForSeq2 Seq.find'             Seq.find
  errorsWheneverThrowsForSeq2 Seq.findBack'         Seq.findBack
  errorsWheneverThrowsForSeq2 Seq.findIndex'        Seq.findIndex
  errorsWheneverThrowsForSeq2 Seq.findIndexBack'    Seq.findIndexBack
  errorsWheneverThrowsForSeq1 Seq.head'             Seq.head
  errorsWheneverThrowsForSeq2 Seq.item'             Seq.item
  errorsWheneverThrowsForSeq1 Seq.last'             Seq.last
  errorsWheneverThrowsForSeq1 Seq.max'<int>         Seq.max<int>
  errorsWheneverThrowsForSeq2 Seq.maxBy'<int, int>  Seq.maxBy<int, int>
  errorsWheneverThrowsForSeq1 Seq.min'<int>         Seq.min<int>
  errorsWheneverThrowsForSeq2 Seq.minBy'<int, int>  Seq.minBy<int, int>
  errorsWheneverThrowsForSeq2 Seq.pick'             Seq.pick
  errorsWheneverThrowsForSeq2 Seq.reduce'           Seq.reduce
  errorsWheneverThrowsForSeq2 Seq.reduceBack'       Seq.reduceBack
  errorsWheneverThrowsForSeq2 Seq.skip'             Seq.skip
  errorsWheneverThrowsForSeq2 Seq.splitInto'        Seq.splitInto
  errorsWheneverThrowsForSeq1 Seq.tail'             Seq.tail
  errorsWheneverThrowsForSeq2 Seq.take'             Seq.take
  errorsWheneverThrowsForSeq2 Seq.windowed'         Seq.windowed

let (|Seq|_|) x = 
  match box x with
  | :? (obj seq) as xs -> Some xs
  | _ -> None

let (|Float|_|) x =
  match box x with
  | :? float as a -> Some a
  | _ -> None

let safeAndUnsafeVersionProduceSameOutput safeVersion unsafeVersion =
  match safeVersion with
  | Ok x -> 
    match (x, Lazy.force unsafeVersion) with
    | (Seq xs, Seq ys) -> Seq.toList xs = Seq.toList ys
    | (Float x, Float y) -> x.Equals y
    | _ -> Lazy.force unsafeVersion = x
  | _ -> true


let alwaysProduceSameOutput1 safeVersion unsafeVersion = 
  let prop inputs =
    safeAndUnsafeVersionProduceSameOutput (safeVersion inputs) (lazy (unsafeVersion inputs))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutput2 safeVersion unsafeVersion = 
  let prop input1 input2 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2) (lazy (unsafeVersion input1 input2))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutput2ExceptNonEmpty safeVersion unsafeVersion = 
  let prop input1 input2 =
    safeAndUnsafeVersionProduceSameOutput 
      (safeVersion input1 input2 |> Result.map (Seq.map Seq.toArray)) 
      (lazy (unsafeVersion input1 input2 |> Seq.map Seq.toArray))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutput3 safeVersion unsafeVersion =
  let prop input1 input2 input3 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2 input3) (lazy (unsafeVersion input1 input2 input3))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutput4 safeVersion unsafeVersion =
  let prop input1 input2 input3 input4 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2 input3 input4) (lazy (unsafeVersion input1 input2 input3 input4))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutputForSeq1 safeVersion unsafeVersion = 
  alwaysProduceSameOutput1 (List.toSeq >> safeVersion) (List.toSeq >> unsafeVersion)

let alwaysProduceSameOutputForSeq2 safeVersion unsafeVersion = 
  alwaysProduceSameOutput2 
    (fun a xs -> safeVersion a (List.toSeq xs))
    (fun a xs -> unsafeVersion a (List.toSeq xs))

let alwaysProduceSameOutputForSeq2ExceptNonEmpty safeVersion unsafeVersion =
  alwaysProduceSameOutput2ExceptNonEmpty
    (fun a xs -> safeVersion a (List.toSeq xs))
    (fun a xs -> unsafeVersion a (List.toSeq xs))

// Collections become lists and tuples become lists of fields, so NonEmpty and plain results compare equal.
let rec private normalize (x: obj) : obj =
  match x with
  | null | :? string -> x
  | :? System.Collections.IEnumerable as xs -> xs |> Seq.cast<obj> |> Seq.map normalize |> List.ofSeq |> box
  | _ when Reflection.FSharpType.IsTuple (x.GetType ()) ->
    Reflection.FSharpValue.GetTupleFields x |> Array.map normalize |> List.ofArray |> box
  | _ -> x

let private outcome (run: unit -> 'a) : Result<obj, unit> =
  try Ok (normalize (box (run ()))) with _ -> Error ()

// COMPARE WITH .Equals, NOT =, SO NaN MATCHES NaN
let private matchesThrowing (safeResult: Result<_, _>) unsafeVersion =
  match safeResult with
  | Ok x -> (outcome unsafeVersion).Equals (Ok (normalize (box x)) : Result<obj, unit>)
  | Error _ -> Result.isError (outcome unsafeVersion)

let private matches nonEmptyVersion plainVersion =
  (outcome nonEmptyVersion).Equals (outcome plainVersion)

// Each property builds its non-empty input from a generated head and tail.
// `ofList` builds the NonEmpty side's collection and `plainOfList` the plain side's.

let nonEmptyMatchesThrowing1 ofList plainOfList safeVersion unsafeVersion =
  Check.QuickThrowOnFailure (fun x xs ->
    matchesThrowing
      (safeVersion (NonEmpty.assume (ofList (x :: xs))))
      (fun () -> unsafeVersion (plainOfList (x :: xs))))

let nonEmptyMatchesThrowing2 ofList plainOfList safeVersion unsafeVersion =
  Check.QuickThrowOnFailure (fun arg x xs ->
    matchesThrowing
      (safeVersion arg (NonEmpty.assume (ofList (x :: xs))))
      (fun () -> unsafeVersion arg (plainOfList (x :: xs))))

let nonEmptyMatches1 ofList plainOfList nonEmptyVersion plainVersion =
  Check.QuickThrowOnFailure (fun x xs ->
    matches
      (fun () -> nonEmptyVersion (NonEmpty.assume (ofList (x :: xs))))
      (fun () -> plainVersion (plainOfList (x :: xs))))

let nonEmptyMatches2 ofList plainOfList nonEmptyVersion plainVersion =
  Check.QuickThrowOnFailure (fun arg x xs ->
    matches
      (fun () -> nonEmptyVersion arg (NonEmpty.assume (ofList (x :: xs))))
      (fun () -> plainVersion arg (plainOfList (x :: xs))))

// The last collection is one longer half the time, so both the equal-length and mismatched paths get exercised.

let nonEmptyMatchesThrowingZip2 ofList plainOfList safeVersion unsafeVersion =
  Check.QuickThrowOnFailure (fun arg (x: _ * _) xys lastIsLonger ->
    let xs = List.map fst (x :: xys)
    let ys = List.map snd (x :: xys) @ (if lastIsLonger then [snd x] else [])
    matchesThrowing
      (safeVersion arg (NonEmpty.assume (ofList xs)) (NonEmpty.assume (ofList ys)))
      (fun () -> unsafeVersion arg (plainOfList xs) (plainOfList ys)))

let private unzip3NonEmpty ((x: _ * _ * _), xyzs) lastIsLonger =
  let (xs, ys, zs) = List.unzip3 (x :: xyzs)
  let (_, _, z) = x
  (xs, ys, zs @ (if lastIsLonger then [z] else []))

let nonEmptyMatchesThrowingZip3 ofList plainOfList safeVersion unsafeVersion =
  Check.QuickThrowOnFailure (fun arg x xyzs lastIsLonger ->
    let (xs, ys, zs) = unzip3NonEmpty (x, xyzs) lastIsLonger
    matchesThrowing
      (safeVersion arg (NonEmpty.assume (ofList xs)) (NonEmpty.assume (ofList ys)) (NonEmpty.assume (ofList zs)))
      (fun () -> unsafeVersion arg (plainOfList xs) (plainOfList ys) (plainOfList zs)))

let nonEmptyMatchesZip3 ofList plainOfList nonEmptyVersion plainVersion =
  Check.QuickThrowOnFailure (fun x xyzs lastIsLonger ->
    let (xs, ys, zs) = unzip3NonEmpty (x, xyzs) lastIsLonger
    matches
      (fun () -> nonEmptyVersion (NonEmpty.assume (ofList xs)) (NonEmpty.assume (ofList ys)) (NonEmpty.assume (ofList zs)))
      (fun () -> plainVersion (plainOfList xs) (plainOfList ys) (plainOfList zs)))

/// Runs `iterate` with an action that records what it's given.
let visits iterate =
  let visited = ResizeArray ()
  iterate visited.Add
  List.ofSeq visited

let visitsOrError iterate =
  let visited = ResizeArray ()
  iterate visited.Add |> Result.map (fun () -> List.ofSeq visited)

let toPositive n = abs (n % 100) + 1
let toNatural n = abs (n % 100)


[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutputForSeq1 averageFloats'       averageFloats
  alwaysProduceSameOutputForSeq2 averageByFloats'     averageByFloats
  alwaysProduceSameOutputForSeq1 Seq.exactlyOne'      Seq.exactlyOne
  alwaysProduceSameOutputForSeq2 Seq.find'            Seq.find
  alwaysProduceSameOutputForSeq2 Seq.findBack'        Seq.findBack
  alwaysProduceSameOutputForSeq2 Seq.findIndex'       Seq.findIndex
  alwaysProduceSameOutputForSeq2 Seq.findIndexBack'   Seq.findIndexBack
  alwaysProduceSameOutputForSeq1 Seq.head'            Seq.head
  alwaysProduceSameOutputForSeq2 Seq.item'            Seq.item
  alwaysProduceSameOutputForSeq1 Seq.last'            Seq.last
  alwaysProduceSameOutputForSeq1 Seq.max'<int>        Seq.max
  alwaysProduceSameOutputForSeq2 Seq.maxBy'<int, int> Seq.maxBy
  alwaysProduceSameOutputForSeq1 Seq.min'<int>        Seq.min
  alwaysProduceSameOutputForSeq2 Seq.minBy'<int, int> Seq.minBy
  alwaysProduceSameOutputForSeq2 Seq.pick'            Seq.pick
  alwaysProduceSameOutputForSeq2 Seq.reduce'          Seq.reduce
  alwaysProduceSameOutputForSeq2 Seq.reduceBack'      Seq.reduceBack
  alwaysProduceSameOutputForSeq2 Seq.skip'            Seq.skip
  alwaysProduceSameOutputForSeq2 Seq.splitInto'       Seq.splitInto
  alwaysProduceSameOutputForSeq1 Seq.tail'            Seq.tail
  alwaysProduceSameOutputForSeq2 Seq.take'            Seq.take

  alwaysProduceSameOutputForSeq2ExceptNonEmpty  Seq.chunkBySize'  Seq.chunkBySize
  alwaysProduceSameOutputForSeq2ExceptNonEmpty  Seq.windowed'     Seq.windowed

[<Test>]
let ``isHungAfter allows elements below the limit`` () =
  test <@ Seq.initInfinite id |> Seq.isHungAfter 10 |> Seq.take 10 |> Seq.toList = [0..9] @>

[<Test>]
let ``isHungAfter throws when the limit is exceeded`` () =
  raises<InfiniteSequenceEvaluationHung>
    <@ Seq.initInfinite id |> Seq.isHungAfter 10 |> Seq.take 11 |> Seq.toList @>

[<Test>]
let ``isHungAfter works with finite sequences that stay within the limit`` () =
  test <@ [1..5] |> Seq.isHungAfter 10 |> Seq.toList = [1..5] @>

[<Test>]
let ``isHungAfter throws for finite sequences that exceed the limit`` () =
  raises<InfiniteSequenceEvaluationHung>
    <@ [1..11] |> Seq.isHungAfter 10 |> Seq.toList @>

module TakeUntilIncluding = 
  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ Seq.takeUntilIncluding (fun _ -> true) Seq.empty |> Seq.toList = [] @>

  [<Test>]
  let ``returns through the first matching element`` () =
    test <@ Seq.takeUntilIncluding ((=) 3) [1;2;3;4;5] |> Seq.toList = [1;2;3] @>

  [<Test>]
  let ``returns only the first element when it matches`` () =
    test <@ Seq.takeUntilIncluding ((=) 3) [3;4;5] |> Seq.toList = [3] @>

  [<Test>]
  let ``stops at the first match even when multiple elements match`` () =
    test <@ Seq.takeUntilIncluding ((=) 3) [1;3;3;3] |> Seq.toList = [1;3] @>

  [<Test>]
  let ``returns the full sequence when no element matches`` () =
    test <@ Seq.takeUntilIncluding ((=) 99) [1;2;3] |> Seq.toList = [1;2;3] @>

  [<Test>]
  let ``works with infinite sequences`` () =
    // stops after finding the matching element rather than diverging
    test <@ Seq.initInfinite id |> Seq.takeUntilIncluding ((=) 3) |> Seq.toList = [0;1;2;3] @>

  [<Test>]
  let ``is lazy - does not evaluate past the matching element`` () =
    let splitInfinite: seq<_> = InfiniteSeq.initBounded 3000 id
    test <@ splitInfinite |> Seq.takeUntilIncluding ((=) 3) |> Seq.toList = [0;1;2;3] @>

module SkipUntilIncluding =
  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ Seq.skipUntilIncluding (fun _ -> true) Seq.empty |> Seq.toList = [] @>

  [<Test>]
  let ``returns elements after the first matching element`` () =
    test <@ Seq.skipUntilIncluding ((=) 3) [1;2;3;4;5] |> Seq.toList = [4;5] @>

  [<Test>]
  let ``returns elements after the first element when it matches`` () =
    test <@ Seq.skipUntilIncluding ((=) 3) [3;4;5] |> Seq.toList = [4;5] @>

  [<Test>]
  let ``stops skipping at the first match even when multiple elements match`` () =
    test <@ Seq.skipUntilIncluding ((=) 3) [1;3;3;3] |> Seq.toList = [3;3] @>

  [<Test>]
  let ``returns empty when the match is the last element`` () =
    test <@ Seq.skipUntilIncluding ((=) 3) [1;2;3] |> Seq.toList = [] @>

  [<Test>]
  let ``returns empty when no element matches`` () =
    test <@ Seq.skipUntilIncluding ((=) 99) [1;2;3] |> Seq.toList = [] @>

  [<Test>]
  let ``works with infinite sequences`` () =
    // yields the infinite tail after the matching element
    test <@ Seq.initInfinite id |> Seq.skipUntilIncluding ((=) 3) |> Seq.take 4 |> Seq.toList = [4;5;6;7] @>

  [<Test>]
  let ``takeUntilIncluding and skipUntilIncluding partition the sequence`` () =
    let xs = [1;2;3;4;5]
    let taken = Seq.takeUntilIncluding ((=) 3) xs |> Seq.toList
    let skipped = Seq.skipUntilIncluding ((=) 3) xs |> Seq.toList
    test <@ taken @ skipped = xs @>

module Splitting =
  let toLists (xs:seq<#seq<_>>) =
    Seq.toList <| Seq.map Seq.toList xs

  [<Test>]
  let ``returns what the documentation says`` () =

    test
      <@
        (Seq.split ((=) 100) [1;2;3;100;100;4;100;5;6] |> toLists)
          = [[1;2;3;100];[100];[4;100];[5;6]]
      @>

    test
      <@
        (Seq.splitPairwise (=) [0;1;1;2;3;4;4;4;5] |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5]]
      @>

    test
      <@
        (Seq.NonEmpty.split ((=) 100) (Seq.NonEmpty.create 1 [2;3;100;100;4;100;5;6]) |> toLists)
          = [[1;2;3;100];[100];[4;100];[5;6]]

        &&

        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [1;1;2;3;4;4;4;5]) |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5]]
      @>

  [<Test>]
  let ``works with infinite lists`` () =
    let splitInfinite = Seq.append [1;2;3;100;100;4;100;5;6] (InfiniteSeq.initBounded 3000 id)
    let neSplitInfinite = NonEmpty.assume splitInfinite
    test
      <@
        (Seq.split ((=) 100) splitInfinite |> Seq.truncate 3 |> toLists)
          = [[1;2;3;100];[100];[4;100]]
      @>

    test
      <@
        (Seq.NonEmpty.split ((=) 100) neSplitInfinite |> Seq.truncate 3 |> toLists)
          = [[1;2;3;100];[100];[4;100]]
      @>

    let pairwiseInfinite = Seq.append [0;1;1;2;3;4;4;4;5;0] (InfiniteSeq.initBounded 3000 id)
    let nePairwiseInfinite = NonEmpty.assume pairwiseInfinite
    test
      <@
        (Seq.splitPairwise (=) pairwiseInfinite |> Seq.truncate 4 |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5;0]]
      @>

    test
      <@
        (Seq.NonEmpty.splitPairwise (=) nePairwiseInfinite |> Seq.truncate 4 |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5;0]]
      @>


  [<Test>]
  let ``inner segments can be infinite`` () =
    // [0; 1; 2; 3; ...]: first segment is [0; 1], then an infinite segment [2; 3; 4; ...]
    // that never triggers the split again
    let splitInfinite: seq<_> = InfiniteSeq.initBounded 3000 id
    let neSplitInfinite = NonEmpty.assume splitInfinite

    test <@ (Seq.split ((=) 1) splitInfinite |> Seq.item 1 |> Seq.truncate 4 |> Seq.toList) = [2; 3; 4; 5] @>
    test <@ (Seq.NonEmpty.split ((=) 1) neSplitInfinite |> Seq.item 1 |> Seq.truncate 4 |> Seq.toList) = [2; 3; 4; 5] @>

    // [5; 5; 0; 1; 2; 3; ...]: one split at (5,5), then an infinite segment [5; 0; 1; 2; 3; ...]
    // with no equal adjacent pairs, so it never splits again
    let pairwiseInfinite = Seq.append [5; 5] (InfiniteSeq.initBounded 3000 id)
    let nePairwiseInfinite = NonEmpty.assume pairwiseInfinite

    test <@ (Seq.splitPairwise (=) pairwiseInfinite |> Seq.item 1 |> Seq.truncate 4 |> Seq.toList) = [5; 0; 1; 2] @>
    test <@ (Seq.NonEmpty.splitPairwise (=) nePairwiseInfinite |> Seq.item 1 |> Seq.truncate 4 |> Seq.toList) = [5; 0; 1; 2] @>

  [<Test>]
  let ``splits empty and single element sequences`` () =
    test
      <@
        (Seq.split ((=) 5) Seq.empty |> toLists) = []
        &&
        (Seq.split ((=) 5) [0] |> toLists) = [[0]]
        &&
        (Seq.split ((=) 5) [5] |> toLists) = [[5]]
        &&
        (Seq.split ((=) 5) [5; 5] |> toLists) = [[5]; [5]]
      @>

    test
      <@
        (Seq.splitPairwise (=) Seq.empty |> toLists) = []
        &&
        (Seq.splitPairwise (=) (Seq.singleton 0) |> toLists) = [[0]]
        &&
        (Seq.splitPairwise (=) [5; 5] |> toLists) = [[5]; [5]]
      @>
  
  [<Test>]
  let ``splits properly for multiple types of inputs`` () =
    test
      <@
        (Seq.split ((=) 5) [] |> toLists) = []
        &&
        (Seq.split ((=) 5) [0] |> toLists) = [[0]]
        &&
        (Seq.split ((=) 5) [5] |> toLists) = [[5]]
        &&
        (Seq.split ((=) 5) [0;5] |> toLists) = [[0; 5]]
        &&
        (Seq.split ((=) 5) [5;5] |> toLists) = [[5]; [5]]
        &&
        (Seq.split ((=) 5) [5;0] |> toLists) = [[5]; [0]]
        &&
        (Seq.split ((=) 5) [5;0;0;5;5;0;5] |> toLists) = [[5]; [0;0;5]; [5]; [0;5]]
      @>

    test
      <@
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.singleton 0) |> toLists) = [[0]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.singleton 5) |> toLists) = [[5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 0 [5]) |> toLists) = [[0; 5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5 [5]) |> toLists) = [[5]; [5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5 [0]) |> toLists) = [[5]; [0]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5 [0;0;5;5;0;5]) |> toLists) = [[5]; [0;0;5]; [5]; [0;5]]
      @>

  [<Test>]
  let ``splits pairwise properly for multiple types of inputs`` () = 
    let bigDiff i j = abs (i - j) > 5
    test 
      <@
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.singleton 0) |> toLists) = [[0]]
        &&
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [1]) |> toLists) = [[0;1]]
        &&
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [0]) |> toLists) = [[0]; [0]]
        &&
        (Seq.NonEmpty.splitPairwise (bigDiff) (Seq.NonEmpty.create 1 [2;12;13;23;24]) |> toLists)
          = [[1;2]; [12;13]; [23;24]]
        &&
        (Seq.NonEmpty.splitPairwise (bigDiff) (Seq.NonEmpty.create 1 [2;12;13;23]) |> toLists)
          = [[1;2]; [12;13]; [23]]
      @>

  [<Test>]
  let ``inner segments remain valid after outer sequence is fully materialized`` () =
    let splitSegments = Seq.split ((=) 100) [1;2;3;100;100;4;100;5;6] |> Seq.toList
    test <@ splitSegments |> List.map Seq.toList = [[1;2;3;100];[100];[4;100];[5;6]] @>

    let neSplitSegments = Seq.NonEmpty.split ((=) 100) (Seq.NonEmpty.create 1 [2;3;100;100;4;100;5;6]) |> Seq.toList
    test <@ neSplitSegments |> List.map Seq.toList = [[1;2;3;100];[100];[4;100];[5;6]] @>

    let segments = Seq.splitPairwise (=) [0;1;1;2;3;4;4;4;5] |> Seq.toList
    test <@ segments |> List.map Seq.toList = [[0;1];[1;2;3;4];[4];[4;5]] @>

    let neSegments = Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [1;1;2;3;4;4;4;5]) |> Seq.toList
    test <@ neSegments |> List.map Seq.toList = [[0;1];[1;2;3;4];[4];[4;5]] @>

  [<Test>]
  let ``inner segments can be re-enumerated`` () =
    let splitFirstSegment = Seq.split ((=) 100) [1;2;3;100;4;100] |> Seq.toList |> List.head
    test
      <@
        Seq.toList splitFirstSegment = [1;2;3;100]
        && Seq.toList splitFirstSegment = [1;2;3;100]
      @>

    let neSplitFirstSegment = Seq.NonEmpty.split ((=) 100) (Seq.NonEmpty.create 1 [2;3;100;4;100]) |> Seq.toList |> List.head
    test
      <@
        Seq.toList neSplitFirstSegment = [1;2;3;100]
        && Seq.toList neSplitFirstSegment = [1;2;3;100]
      @>

    let firstSegment = Seq.splitPairwise (=) [0;1;1;2] |> Seq.toList |> List.head
    test
      <@
        Seq.toList firstSegment = [0;1]
        && Seq.toList firstSegment = [0;1]
      @>

    let neFirstSegment = Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [1;1;2]) |> Seq.toList |> List.head
    test
      <@
        Seq.toList neFirstSegment = [0;1]
        && Seq.toList neFirstSegment = [0;1]
      @>

  [<Test>]
  let ``inner segments can be consumed out of order`` () =
    let splitSegments = Seq.split ((=) 5) [5;0;0;5;5;0;5] |> Seq.toArray
    test
      <@
        Seq.toList splitSegments.[2] = [5]
        && Seq.toList splitSegments.[0] = [5]
        && Seq.toList splitSegments.[3] = [0;5]
        && Seq.toList splitSegments.[1] = [0;0;5]
      @>

    let neSplitSegments = Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5 [0;0;5;5;0;5]) |> Seq.toArray
    test
      <@
        Seq.toList neSplitSegments.[2] = [5]
        && Seq.toList neSplitSegments.[0] = [5]
        && Seq.toList neSplitSegments.[3] = [0;5]
        && Seq.toList neSplitSegments.[1] = [0;0;5]
      @>

    let segments = Seq.splitPairwise (=) [0;1;1;2;3;4;4;4;5] |> Seq.toArray
    test
      <@
        Seq.toList segments.[2] = [4]
        && Seq.toList segments.[0] = [0;1]
        && Seq.toList segments.[3] = [4;5]
        && Seq.toList segments.[1] = [1;2;3;4]
      @>

    let neSegments = Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0 [1;1;2;3;4;4;4;5]) |> Seq.toArray
    test
      <@
        Seq.toList neSegments.[2] = [4]
        && Seq.toList neSegments.[0] = [0;1]
        && Seq.toList neSegments.[3] = [4;5]
        && Seq.toList neSegments.[1] = [1;2;3;4]
      @>

[<Test>]
let ``zips multiple sequences together via computation expression`` () =
  let xs = [1;2;3;4;5]
  let ys = [|10;20;30;40;50;60|]
  let zs = Seq.initInfinite id

  let result = 
    Seq.zipper {
      let! x = xs
      and! y = ys 
      and! z = zs 
      return x + y + z
    }

  test <@ List.ofSeq result = [11;23;35;47;59] @>

  let xs: NonEmptySeq<_> = NonEmpty.assume [1;2;3;4;5]
  let ys: NonEmptySeq<_> = NonEmpty.assume [|10;20;30;40;50;60|]
  let zs: NonEmptySeq<_> = NonEmpty.assume <| Seq.initInfinite id

  let result = 
    Seq.NonEmpty.zipper {
      let! x = xs
      and! y = ys 
      and! z = zs 
      return x + y + z
    }

  test <@ List.ofSeq result = [11;23;35;47;59] @>

module NonEmptyParity =
  let sameOrBothFail1 safeVersion unsafeVersion = nonEmptyMatchesThrowing1 List.toSeq List.toSeq safeVersion unsafeVersion
  let sameOrBothFail2 safeVersion unsafeVersion = nonEmptyMatchesThrowing2 List.toSeq List.toSeq safeVersion unsafeVersion
  let same1 nonEmptyVersion plainVersion = nonEmptyMatches1 List.toSeq List.toSeq nonEmptyVersion plainVersion
  let same2 nonEmptyVersion plainVersion = nonEmptyMatches2 List.toSeq List.toSeq nonEmptyVersion plainVersion

  [<Test>]
  let ``Seq.NonEmpty functions error whenever the Seq versions throw, and otherwise match them`` () =
    sameOrBothFail2 Seq.NonEmpty.chunkBySize'                                        Seq.chunkBySize
    sameOrBothFail2 (fun n -> Seq.NonEmpty.tryChunkBySize n >> Option.toResult ())   Seq.chunkBySize
    sameOrBothFail1 Seq.NonEmpty.exactlyOne'                                         Seq.exactlyOne
    sameOrBothFail1 (Seq.NonEmpty.tryExactlyOne >> Option.toResult ())               Seq.exactlyOne
    sameOrBothFail2 Seq.NonEmpty.find'                                               Seq.find
    sameOrBothFail2 Seq.NonEmpty.findBack'                                           Seq.findBack
    sameOrBothFail2 Seq.NonEmpty.findIndex'                                          Seq.findIndex
    sameOrBothFail2 Seq.NonEmpty.findIndexBack'                                      Seq.findIndexBack
    sameOrBothFail2 Seq.NonEmpty.item'                                               Seq.item
    sameOrBothFail2 Seq.NonEmpty.pick'                                               Seq.pick
    sameOrBothFail2 Seq.NonEmpty.splitInto'                                          Seq.splitInto
    sameOrBothFail2 (fun n -> Seq.NonEmpty.trySplitInto n >> Option.toResult ())     Seq.splitInto
    sameOrBothFail2 Seq.NonEmpty.windowed'                                           Seq.windowed
    sameOrBothFail2 (fun n -> Seq.NonEmpty.tryWindowed n >> Option.toResult ())      Seq.windowed

  [<Test>]
  let ``Seq.NonEmpty functions always match the Seq versions`` () =
    same2 (fun n -> Seq.NonEmpty.chunksOf (PositiveInt.assume (toPositive n)))    (fun n -> Seq.chunkBySize (toPositive n))
    same2 (fun n -> Seq.NonEmpty.splitIntoN (PositiveInt.assume (toPositive n)))  (fun n -> Seq.splitInto (toPositive n))
    same2 (fun n -> Seq.NonEmpty.window (PositiveInt.assume (toPositive n)))      (fun n -> Seq.windowed (toPositive n))
    same2 (fun n -> Seq.NonEmpty.drop (NaturalInt.assume (toNatural n)))          (fun n xs -> Seq.skip (min (toNatural n) (Seq.length xs)) xs)
    same2 Seq.NonEmpty.dropLenient                                                (fun n xs -> Seq.skip (max 0 (min n (Seq.length xs))) xs)
    same2 (fun (f: _ -> float) -> Seq.NonEmpty.averageBy f)                       (fun f -> Seq.averageBy f)
    same2 Seq.NonEmpty.contains                                                   Seq.contains
    same2 Seq.NonEmpty.countBy                                                    Seq.countBy
    same2 Seq.NonEmpty.exists                                                     Seq.exists
    same2 Seq.NonEmpty.forall                                                     Seq.forall
    same2 Seq.NonEmpty.groupBy                                                    Seq.groupBy
    same2 (fun (f, s) xs -> Seq.NonEmpty.foldBack f xs s)                         (fun (f, s) xs -> Seq.foldBack f xs s)
    same1 Seq.NonEmpty.last                                                       Seq.last
    same2 Seq.NonEmpty.reduceBack                                                 Seq.reduceBack
    same2 Seq.NonEmpty.skipUntilIncluding                                         Seq.skipUntilIncluding
    same1 (Seq.NonEmpty.sort : NonEmptySeq<int> -> _)                             Seq.sort
    same2 (fun (f: _ -> int) -> Seq.NonEmpty.sortBy f)                            (fun f -> Seq.sortBy f)
    same1 (Seq.NonEmpty.sortDescending : NonEmptySeq<int> -> _)                   Seq.sortDescending
    same2 (fun (f: _ -> int) -> Seq.NonEmpty.sortByDescending f)                  (fun f -> Seq.sortByDescending f)
    same2 Seq.NonEmpty.sortWith                                                   Seq.sortWith
    same2 Seq.NonEmpty.tryFindBack                                                Seq.tryFindBack
    same2 Seq.NonEmpty.tryFindIndex                                               Seq.tryFindIndex
    same2 Seq.NonEmpty.tryFindIndexBack                                           Seq.tryFindIndexBack
    same2 Seq.NonEmpty.tryPick                                                    Seq.tryPick
    same1 (fun xs -> (Seq.NonEmpty.sum xs : int))                                 Seq.sum<int>
    same2 (fun (f: _ -> int) -> Seq.NonEmpty.sumBy f)                             (fun f -> Seq.sumBy f)
    same2 Seq.NonEmpty.choose                                                     Seq.choose
    same2 Seq.NonEmpty.takeWhile                                                  Seq.takeWhile
    same2 Seq.NonEmpty.skipWhile                                                  Seq.skipWhile
    same2 (fun (f, s) xs -> Seq.NonEmpty.scanBack f xs s)                         (fun (f, s) xs -> Seq.scanBack f xs s)
    same2 (fun (f, s) -> Seq.NonEmpty.mapFold f s)                                (fun (f, s) -> Seq.mapFold f s)
    same2 (fun (f, s) xs -> Seq.NonEmpty.mapFoldBack f xs s)                      (fun (f, s) xs -> Seq.mapFoldBack f xs s)
    same1 (fun xs -> visits (fun visit -> Seq.NonEmpty.iter visit xs))           (fun xs -> visits (fun visit -> Seq.iter visit xs))
    same1
      (fun xs -> visits (fun visit -> Seq.NonEmpty.iteri (fun i x -> visit (i, x)) xs))
      (fun xs -> visits (fun visit -> Seq.iteri (fun i x -> visit (i, x)) xs))
    nonEmptyMatchesZip3 List.toSeq List.toSeq Seq.NonEmpty.zip3 Seq.zip3

  [<Test>]
  let ``zip3 stops at the shortest input, even when another is infinite`` () =
    let infinite : NonEmptySeq<_> = NonEmpty.assume (Seq.initInfinite id)
    let result = Seq.NonEmpty.zip3 (NonEmpty.assume [3;1;2]) (NonEmpty.assume [10;20]) infinite
    test <@ Seq.NonEmpty.toList result = [(3,10,0);(1,20,1)] @>
