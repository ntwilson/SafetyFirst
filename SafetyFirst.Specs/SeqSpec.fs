module SafetyFirst.Specs.SeqSpec

open NUnit.Framework
open FsCheck

open SafetyFirst

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


[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutputForSeq1 averageFloats'       averageFloats
  alwaysProduceSameOutputForSeq2 averageByFloats'     averageByFloats
  alwaysProduceSameOutputForSeq2 Seq.chunkBySize'     Seq.chunkBySize
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
  alwaysProduceSameOutputForSeq2 Seq.windowed'        Seq.windowed


