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

let errorsWheneverThrows1 (safeVersion : _ list -> _) (unsafeVersion : _ list -> _) = 
  let prop inputs  =
    errorsAndThrowsOrNeither (safeVersion inputs) (lazy (unsafeVersion inputs))

  Check.QuickThrowOnFailure prop

let errorsWheneverThrows2 (safeVersion : _ -> _ list -> _) (unsafeVersion : _ -> _ list -> _) = 
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

let averageFloats' (xs:float seq) = Seq.average' xs
let averageFloats (xs:float seq) = Seq.average xs
let averageByFloats' (projection:_ -> float) xs = Seq.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = Seq.averageBy projection xs

[<Test>]
let ``Safe Seq functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrows1 averageFloats'        averageFloats
  errorsWheneverThrows2 averageByFloats'      averageByFloats
  errorsWheneverThrows2 Seq.chunkBySize'      Seq.chunkBySize
  errorsWheneverThrows1 Seq.exactlyOne'       Seq.exactlyOne
  errorsWheneverThrows2 Seq.find'             Seq.find
  errorsWheneverThrows2 Seq.findBack'         Seq.findBack
  errorsWheneverThrows2 Seq.findIndex'        Seq.findIndex
  errorsWheneverThrows2 Seq.findIndexBack'    Seq.findIndexBack
  errorsWheneverThrows1 Seq.head'             Seq.head
  errorsWheneverThrows2 Seq.item'             Seq.item
  errorsWheneverThrows1 Seq.last'             Seq.last
  errorsWheneverThrows1 Seq.max'<int>         Seq.max<int>
  errorsWheneverThrows2 Seq.maxBy'<int, int>  Seq.maxBy<int, int>
  errorsWheneverThrows1 Seq.min'<int>         Seq.min<int>
  errorsWheneverThrows2 Seq.minBy'<int, int>  Seq.minBy<int, int>
  errorsWheneverThrows2 Seq.pick'             Seq.pick
  errorsWheneverThrows2 Seq.reduce'           Seq.reduce
  errorsWheneverThrows2 Seq.reduceBack'       Seq.reduceBack
  errorsWheneverThrows2 Seq.skip'             Seq.skip
  errorsWheneverThrows2 Seq.splitInto'        Seq.splitInto
  errorsWheneverThrows1 Seq.tail'             Seq.tail
  errorsWheneverThrows2 Seq.take'             Seq.take
  errorsWheneverThrows2 Seq.windowed'         Seq.windowed

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


let alwaysProduceSameOutput1 (safeVersion : _ list -> _) (unsafeVersion : _ list -> _) = 
  let prop inputs =
    safeAndUnsafeVersionProduceSameOutput (safeVersion inputs) (lazy (unsafeVersion inputs))

  Check.QuickThrowOnFailure prop

let alwaysProduceSameOutput2 (safeVersion : _ -> _ list -> _) (unsafeVersion : _ -> _ list -> _) = 
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


[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 averageFloats'       averageFloats
  alwaysProduceSameOutput2 averageByFloats'     averageByFloats
  alwaysProduceSameOutput2 Seq.chunkBySize'     Seq.chunkBySize
  alwaysProduceSameOutput1 Seq.exactlyOne'      Seq.exactlyOne
  alwaysProduceSameOutput2 Seq.find'            Seq.find
  alwaysProduceSameOutput2 Seq.findBack'        Seq.findBack
  alwaysProduceSameOutput2 Seq.findIndex'       Seq.findIndex
  alwaysProduceSameOutput2 Seq.findIndexBack'   Seq.findIndexBack
  alwaysProduceSameOutput1 Seq.head'            Seq.head
  alwaysProduceSameOutput2 Seq.item'            Seq.item
  alwaysProduceSameOutput1 Seq.last'            Seq.last
  alwaysProduceSameOutput1 Seq.max'<int>        Seq.max
  alwaysProduceSameOutput2 Seq.maxBy'<int, int> Seq.maxBy
  alwaysProduceSameOutput1 Seq.min'<int>        Seq.min
  alwaysProduceSameOutput2 Seq.minBy'<int, int> Seq.minBy
  alwaysProduceSameOutput2 Seq.pick'            Seq.pick
  alwaysProduceSameOutput2 Seq.reduce'          Seq.reduce
  alwaysProduceSameOutput2 Seq.reduceBack'      Seq.reduceBack
  alwaysProduceSameOutput2 Seq.skip'            Seq.skip
  alwaysProduceSameOutput2 Seq.splitInto'       Seq.splitInto
  alwaysProduceSameOutput1 Seq.tail'            Seq.tail
  alwaysProduceSameOutput2 Seq.take'            Seq.take
  alwaysProduceSameOutput2 Seq.windowed'        Seq.windowed


