module SafetyFirst.Specs.SeqSpec

open NUnit.Framework
open FsCheck
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Numbers

let check prop =
  Arb.register<Generators.Numbers>() |> ignore
  Check.QuickThrowOnFailure prop

let upcastNE = Seq.NonEmpty.toSeq

let initByInts' = (fun count initializer -> Seq.init' count (NaturalInt.value >> initializer))

[<AutoOpen>]
module SimplifiedFunctions =
  let initByIntsNonEmpty = (fun count initializer -> Seq.NonEmpty.initN count (NaturalInt.value >> initializer))

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

  check prop

let errorsWheneverThrows2 safeVersion unsafeVersion = 
  let prop input1 input2 =
    errorsAndThrowsOrNeither (safeVersion input1 input2) (lazy (unsafeVersion input1 input2))

  check prop

let errorsWheneverThrows3 safeVersion unsafeVersion = 
  let prop input1 input2 input3 =
    errorsAndThrowsOrNeither (safeVersion input1 input2 input3) (lazy (unsafeVersion input1 input2 input3))

  check prop

let errorsWheneverThrows4 safeVersion unsafeVersion = 
  let prop input1 input2 input3 input4 =
    errorsAndThrowsOrNeither (safeVersion input1 input2 input3 input4) (lazy (unsafeVersion input1 input2 input3 input4))

  check prop

let errorsWheneverThrowsForSeq1 safeVersion unsafeVersion = 
  errorsWheneverThrows1 (List.toSeq >> safeVersion) (List.toSeq >> unsafeVersion)

let errorsWheneverThrowsForSeq2 safeVersion unsafeVersion = 
  errorsWheneverThrows2 
    (fun a xs -> safeVersion a (List.toSeq xs))
    (fun a xs -> unsafeVersion a (List.toSeq xs))

[<Test>]
let ``Safe Seq functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrowsForSeq2 Seq.chunkBySize'      Seq.chunkBySize
  errorsWheneverThrowsForSeq1 Seq.exactlyOne'       Seq.exactlyOne
  errorsWheneverThrowsForSeq2 Seq.find'             Seq.find
  errorsWheneverThrowsForSeq2 Seq.findIndex'        Seq.findIndex
  errorsWheneverThrowsForSeq1 Seq.head'             Seq.head
  errorsWheneverThrows2       initByInts'           Seq.init
  errorsWheneverThrowsForSeq2 Seq.item'             Seq.item
  errorsWheneverThrowsForSeq2 Seq.pick'             Seq.pick
  errorsWheneverThrows2       Seq.replicate'        Seq.replicate
  errorsWheneverThrowsForSeq2 Seq.skip'             Seq.skip
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

let areSameOutput =
  function
  | (Seq xs, Seq ys) -> Seq.toList xs = Seq.toList ys
  | (Float x, Float y) -> x.Equals y
  | (x, y) -> x = y

let safeAndUnsafeVersionProduceSameOutput safeVersion unsafeVersion =
  match safeVersion with
  | Ok x -> areSameOutput (x, Lazy.force unsafeVersion)
  | _ -> true


let alwaysProduceSameOutput1 safeVersion unsafeVersion = 
  let prop inputs =
    safeAndUnsafeVersionProduceSameOutput (safeVersion inputs) (lazy (unsafeVersion inputs))

  check prop

let alwaysProduceSameOutput2 safeVersion unsafeVersion = 
  let prop input1 input2 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2) (lazy (unsafeVersion input1 input2))

  check prop

let alwaysProduceSameOutput3 safeVersion unsafeVersion =
  let prop input1 input2 input3 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2 input3) (lazy (unsafeVersion input1 input2 input3))

  check prop

let alwaysProduceSameOutput4 safeVersion unsafeVersion =
  let prop input1 input2 input3 input4 =
    safeAndUnsafeVersionProduceSameOutput (safeVersion input1 input2 input3 input4) (lazy (unsafeVersion input1 input2 input3 input4))

  check prop

let alwaysProduceSameOutputForSeq1 safeVersion unsafeVersion = 
  alwaysProduceSameOutput1 (List.toSeq >> safeVersion) (List.toSeq >> unsafeVersion)

let alwaysProduceSameOutputForSeq2 safeVersion unsafeVersion = 
  alwaysProduceSameOutput2 
    (fun a xs -> safeVersion a (List.toSeq xs))
    (fun a xs -> unsafeVersion a (List.toSeq xs))

[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutputForSeq2 Seq.chunkBySize'     Seq.chunkBySize
  alwaysProduceSameOutputForSeq1 Seq.exactlyOne'      Seq.exactlyOne
  alwaysProduceSameOutputForSeq2 Seq.find'            Seq.find
  alwaysProduceSameOutputForSeq2 Seq.findIndex'       Seq.findIndex
  alwaysProduceSameOutputForSeq1 Seq.head'            Seq.head
  alwaysProduceSameOutput2       initByInts'          Seq.init
  alwaysProduceSameOutputForSeq2 Seq.item'            Seq.item
  alwaysProduceSameOutputForSeq2 Seq.pick'            Seq.pick
  alwaysProduceSameOutput2       Seq.replicate'       Seq.replicate
  alwaysProduceSameOutputForSeq2 Seq.skip'            Seq.skip
  alwaysProduceSameOutputForSeq1 Seq.tail'            Seq.tail
  alwaysProduceSameOutputForSeq2 Seq.take'            Seq.take
  alwaysProduceSameOutputForSeq2 Seq.windowed'        Seq.windowed

module SafeByType =
  let alwaysProduceSameOutput1 safeVersion baseVersion = 
    let prop inputs =
      areSameOutput (safeVersion inputs, baseVersion inputs)

    check prop

  let alwaysProduceSameOutput2 safeVersion baseVersion = 
    let prop input1 input2 =
      areSameOutput (safeVersion input1 input2, baseVersion input1 input2)

    check prop

  let alwaysProduceSameOutput3 safeVersion baseVersion =
    let prop input1 input2 input3 =
      areSameOutput (safeVersion input1 input2 input3, baseVersion input1 input2 input3)

    check prop

  let alwaysProduceSameOutput4 safeVersion baseVersion =
    let prop input1 input2 input3 input4 =
      areSameOutput (safeVersion input1 input2 input3 input4, baseVersion input1 input2 input3 input4)

    check prop

  [<Test>]
  let ``Seq functions that are safe by type behave like the base functions`` () =
    let (>>>) f g = (fun a b -> f a b |> g)

    alwaysProduceSameOutput2  (initByIntsNonEmpty >>> upcastNE)       (Seq.init << PositiveInt.value)
    alwaysProduceSameOutput2  (Seq.NonEmpty.replicateN >>> upcastNE)  (Seq.replicate << PositiveInt.value)

module Infinite =
  let alwaysProduceSameInfiniteOutput actualVersion expectedVersion =
    let sufficientlyInfinite = 20
  
    let prop initialization =
      let makeComparable s = s |> Seq.truncate sufficientlyInfinite |> Seq.toList

      (actualVersion initialization |> makeComparable) 
        = (expectedVersion initialization |> makeComparable)

    Check.QuickThrowOnFailure prop

  [<Test>]
  let ``Infinite Seq initializers behave like Seq initInfinite`` () =
    let ``InfiniteSeq.init`` = (fun initializer -> InfiniteSeq.init (NaturalInt.value >> initializer))
    let ``Seq.NonEmpty.initInfinitely`` = (fun initializer -> Seq.NonEmpty.initInfinitely (NaturalInt.value >> initializer))

    let ``Seq.replicateInfinitely`` initial = Seq.initInfinite (fun _ -> initial)

    alwaysProduceSameInfiniteOutput ``InfiniteSeq.init``                  Seq.initInfinite
    alwaysProduceSameInfiniteOutput   InfiniteSeq.replicate             ``Seq.replicateInfinitely``
    alwaysProduceSameInfiniteOutput ``Seq.NonEmpty.initInfinitely``       Seq.initInfinite
    alwaysProduceSameInfiniteOutput   Seq.NonEmpty.replicateInfinitely  ``Seq.replicateInfinitely``

module Splitting = 
  let ofNonEmpty (xs:seq<#seq<_>>) = 
    Seq.toList <| Seq.map Seq.toList xs

  [<Test>]
  let ``returns what the documentation says`` () =

    test 
      <@
        (Seq.NonEmpty.split ((=) 100) (Seq.NonEmpty.create 1[2;3;100;100;4;100;5;6]) |> ofNonEmpty)
          = [[1;2;3;100];[100];[4;100];[5;6]]

        &&

        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0[1;1;2;3;4;4;4;5]) |> ofNonEmpty)
          = [[0;1];[1;2;3;4];[4];[4;5]]
      @>
  
  [<Test>]
  let ``splits properly for multiple types of inputs`` () = 
    test 
      <@
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.singleton 0) |> ofNonEmpty) = [[0]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.singleton 5) |> ofNonEmpty) = [[5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 0[5]) |> ofNonEmpty) = [[0; 5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5[5]) |> ofNonEmpty) = [[5]; [5]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5[0]) |> ofNonEmpty) = [[5]; [0]]
        &&
        (Seq.NonEmpty.split ((=) 5) (Seq.NonEmpty.create 5[0;0;5;5;0;5]) |> ofNonEmpty) = [[5]; [0;0;5]; [5]; [0;5]]
      @>

  [<Test>]
  let ``splits pairwise properly for multiple types of inputs`` () = 
    let bigDiff i j = abs (i - j) > 5
    test 
      <@
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.singleton 0) |> ofNonEmpty) = [[0]]
        &&
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0[1]) |> ofNonEmpty) = [[0;1]]
        &&
        (Seq.NonEmpty.splitPairwise (=) (Seq.NonEmpty.create 0[0]) |> ofNonEmpty) = [[0]; [0]]
        &&
        (Seq.NonEmpty.splitPairwise (bigDiff) (Seq.NonEmpty.create 1[2;12;13;23;24]) |> ofNonEmpty)
          = [[1;2]; [12;13]; [23;24]]
        &&
        (Seq.NonEmpty.splitPairwise (bigDiff) (Seq.NonEmpty.create 1[2;12;13;23]) |> ofNonEmpty)
          = [[1;2]; [12;13]; [23]]
      @>

  [<Test>]
  let ``splits infinite sequences without hanging`` () = 
    let alwaysFalse (_:int) = false
    test 
      <@
        InfiniteSeq.split (fun i -> i % 20 = 0) (InfiniteSeq.init NaturalInt.value) |> InfiniteSeq.take 3 |> ofNonEmpty
          = [[0]; [1 .. 20]; [21 .. 40]]

        &&

        InfiniteSeq.splitPairwise (fun left right -> left % 20 = 0) (InfiniteSeq.init NaturalInt.value) |> InfiniteSeq.take 3 |> ofNonEmpty
          = [[0]; [1 .. 20]; [21 .. 40]]

        &&

        InfiniteSeq.split alwaysFalse (InfiniteSeq.init NaturalInt.value) |> InfiniteSeq.head |> InfiniteSeq.take 40 |> FSeq.toList
          = [0 .. 39]

        &&

        InfiniteSeq.splitPairwise (fun left right -> false) (InfiniteSeq.init NaturalInt.value) |> InfiniteSeq.head |> InfiniteSeq.take 40 |> FSeq.toList 
          = [0 .. 39]        
      @>
