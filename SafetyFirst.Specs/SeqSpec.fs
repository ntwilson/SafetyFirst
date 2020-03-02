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

[<Test>]
let ``Safe Seq functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrowsForSeq2 Seq.chunkBySize'      Seq.chunkBySize
  errorsWheneverThrowsForSeq1 Seq.exactlyOne'       Seq.exactlyOne
  errorsWheneverThrowsForSeq2 Seq.find'             Seq.find
  errorsWheneverThrowsForSeq2 Seq.findIndex'        Seq.findIndex
  errorsWheneverThrowsForSeq1 Seq.head'             Seq.head
  errorsWheneverThrowsForSeq2 Seq.item'             Seq.item
  errorsWheneverThrowsForSeq2 Seq.pick'             Seq.pick
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
    


[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutputForSeq1 Seq.exactlyOne'      Seq.exactlyOne
  alwaysProduceSameOutputForSeq2 Seq.find'            Seq.find
  alwaysProduceSameOutputForSeq2 Seq.findIndex'       Seq.findIndex
  alwaysProduceSameOutputForSeq1 Seq.head'            Seq.head
  alwaysProduceSameOutputForSeq2 Seq.item'            Seq.item
  alwaysProduceSameOutputForSeq2 Seq.pick'            Seq.pick
  alwaysProduceSameOutputForSeq2 Seq.skip'            Seq.skip
  alwaysProduceSameOutputForSeq1 Seq.tail'            Seq.tail
  alwaysProduceSameOutputForSeq2 Seq.take'            Seq.take

  alwaysProduceSameOutputForSeq2ExceptNonEmpty  Seq.chunkBySize'  Seq.chunkBySize
  alwaysProduceSameOutputForSeq2ExceptNonEmpty  Seq.windowed'     Seq.windowed

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

