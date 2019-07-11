module SafetyFirst.Specs.ListSpec

open NUnit.Framework
open FsCheck

open SafetyFirst
open SafetyFirst.Specs.SeqSpec
open SafetyFirst.Numbers

let upcastNE = List.NonEmpty.toList

let averageFloats' (xs:float list) = List.average' xs
let averageFloats (xs:float list) = List.average xs
let averageByFloats' (projection:_ -> float) xs = List.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = List.averageBy projection xs

let initByInts' =        (fun count initializer -> List.init'          count (NaturalInt.value >> initializer))
let initByIntsNonEmpty = (fun count initializer -> List.NonEmpty.initN count (NaturalInt.value >> initializer))

let arrayTransposeComparable xs = 
  Array.transpose xs
  |> Seq.map (Array.toList) |> Seq.toList

let listTransposeComparable xs =
  Seq.map (Array.toList) xs
  |> List.transpose'

[<Test>]
let ``Safe List functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrows1 averageFloats'                averageFloats
  errorsWheneverThrows2 averageByFloats'              averageByFloats
  errorsWheneverThrows2 List.chunkBySize'             List.chunkBySize
  errorsWheneverThrows1 List.exactlyOne'              List.exactlyOne
  errorsWheneverThrows2 List.find'                    List.find
  errorsWheneverThrows2 List.findBack'                List.findBack
  errorsWheneverThrows2 List.findIndex'               List.findIndex
  errorsWheneverThrows2 List.findIndexBack'           List.findIndexBack
  errorsWheneverThrows4 List.fold2'                   List.fold2
  errorsWheneverThrows4 List.foldBack2'               List.foldBack2
  errorsWheneverThrows3 List.forall2'                 List.forall2
  errorsWheneverThrows1 List.head'                    List.head
  errorsWheneverThrows2 initByInts'                   List.init
  errorsWheneverThrows2 List.item'                    List.item
  errorsWheneverThrows1 List.last'                    List.last
  errorsWheneverThrows3 List.map2'                    List.map2
  errorsWheneverThrows3 List.mapi2'                   List.mapi2
  errorsWheneverThrows4 List.map3'                    List.map3
  errorsWheneverThrows1 List.max'<int>                List.max<int>
  errorsWheneverThrows2 List.maxBy'<int, int>         List.maxBy<int, int>
  errorsWheneverThrows1 List.min'<int>                List.min<int>
  errorsWheneverThrows2 List.minBy'<int, int>         List.minBy<int, int>
  errorsWheneverThrows2 List.pick'                    List.pick
  errorsWheneverThrows2 List.reduce'                  List.reduce
  errorsWheneverThrows2 List.reduceBack'              List.reduceBack
  errorsWheneverThrows2 List.replicate'               List.replicate
  errorsWheneverThrows2 List.skip'                    List.skip
  errorsWheneverThrows2 List.splitAt'                 List.splitAt
  errorsWheneverThrows2 List.splitInto'               List.splitInto
  errorsWheneverThrows1 List.tail'                    List.tail
  errorsWheneverThrows2 List.take'                    List.take
  errorsWheneverThrowsForSeq1 listTransposeComparable arrayTransposeComparable
  errorsWheneverThrows2 List.windowed'                List.windowed
  errorsWheneverThrows2 List.zip'                     List.zip
  errorsWheneverThrows3 List.zip3'                    List.zip3


[<Test>]
let ``Safe List functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 averageFloats'                 averageFloats
  alwaysProduceSameOutput2 averageByFloats'               averageByFloats
  alwaysProduceSameOutput2 List.chunkBySize'              List.chunkBySize
  alwaysProduceSameOutput1 List.exactlyOne'               List.exactlyOne
  alwaysProduceSameOutput2 List.find'                     List.find
  alwaysProduceSameOutput2 List.findBack'                 List.findBack
  alwaysProduceSameOutput2 List.findIndex'                List.findIndex
  alwaysProduceSameOutput2 List.findIndexBack'            List.findIndexBack
  alwaysProduceSameOutput4 List.fold2'                    List.fold2
  alwaysProduceSameOutput4 List.foldBack2'                List.foldBack2
  alwaysProduceSameOutput3 List.forall2'                  List.forall2 
  alwaysProduceSameOutput1 List.head'                     List.head
  alwaysProduceSameOutput2 initByInts'                    List.init
  alwaysProduceSameOutput2 List.item'                     List.item
  alwaysProduceSameOutput1 List.last'                     List.last
  alwaysProduceSameOutput3 List.map2'                     List.map2
  alwaysProduceSameOutput3 List.mapi2'                    List.mapi2
  alwaysProduceSameOutput4 List.map3'                     List.map3
  alwaysProduceSameOutput1 List.max'<int>                 List.max<int>
  alwaysProduceSameOutput2 List.maxBy'<int, int>          List.maxBy<int, int>
  alwaysProduceSameOutput1 List.min'<int>                 List.min<int>
  alwaysProduceSameOutput2 List.minBy'<int, int>          List.minBy<int, int>
  alwaysProduceSameOutput2 List.pick'                     List.pick
  alwaysProduceSameOutput2 List.reduce'                   List.reduce
  alwaysProduceSameOutput2 List.reduceBack'               List.reduceBack
  alwaysProduceSameOutput2 List.replicate'                List.replicate
  alwaysProduceSameOutput2 List.skip'                     List.skip
  alwaysProduceSameOutput2 List.splitAt'                  List.splitAt
  alwaysProduceSameOutput2 List.splitInto'                List.splitInto
  alwaysProduceSameOutput1 List.tail'                     List.tail
  alwaysProduceSameOutput2 List.take'                     List.take
  alwaysProduceSameOutputForSeq1 listTransposeComparable  arrayTransposeComparable
  alwaysProduceSameOutput2 List.windowed'                 List.windowed
  alwaysProduceSameOutput2 List.zip'                      List.zip
  alwaysProduceSameOutput3 List.zip3'                     List.zip3

module SafeByType =
  open SeqSpec.SafeByType

  [<Test>]
  let ``List functions that are safe by type behave like the base functions`` () =
    let (>>>) f g = (fun a b -> f a b |> g)

    alwaysProduceSameOutput2  (initByIntsNonEmpty >>> upcastNE)        (List.init << PositiveInt.value)
    alwaysProduceSameOutput2  (List.NonEmpty.replicateN >>> upcastNE)  (List.replicate << PositiveInt.value)