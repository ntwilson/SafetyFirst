module SafetyFirst.Specs.ListSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec

let averageFloats' (xs:float list) = List.average' xs
let averageFloats (xs:float list) = List.average xs
let averageByFloats' (projection:_ -> float) xs = List.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = List.averageBy projection xs

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
  alwaysProduceSameOutput1 List.exactlyOne'               List.exactlyOne
  alwaysProduceSameOutput2 List.find'                     List.find
  alwaysProduceSameOutput2 List.findBack'                 List.findBack
  alwaysProduceSameOutput2 List.findIndex'                List.findIndex
  alwaysProduceSameOutput2 List.findIndexBack'            List.findIndexBack
  alwaysProduceSameOutput4 List.fold2'                    List.fold2
  alwaysProduceSameOutput4 List.foldBack2'                List.foldBack2
  alwaysProduceSameOutput3 List.forall2'                  List.forall2 
  alwaysProduceSameOutput1 List.head'                     List.head
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
  alwaysProduceSameOutput2 List.skip'                     List.skip
  alwaysProduceSameOutput2 List.splitAt'                  List.splitAt
  alwaysProduceSameOutput1 List.tail'                     List.tail
  alwaysProduceSameOutput2 List.take'                     List.take
  alwaysProduceSameOutputForSeq1 listTransposeComparable  arrayTransposeComparable
  alwaysProduceSameOutput2 List.zip'                      List.zip
  alwaysProduceSameOutput3 List.zip3'                     List.zip3


  alwaysProduceSameOutput2ExceptNonEmpty  List.chunkBySize'  List.chunkBySize
  alwaysProduceSameOutput2ExceptNonEmpty  List.splitInto'    List.splitInto
  alwaysProduceSameOutput2ExceptNonEmpty  List.windowed'     List.windowed


[<Test>]
let ``zips multiple lists together via computation expression`` () =
  let xs = [1;2;3;4;5]
  let ys = [10;20;30;40;50;60]
  let zs = [0 .. 100]

  let result = 
    List.zipper {
      let! x = xs
      and! y = ys 
      and! z = zs 
      return x + y + z
    }

  test <@ result = [11;23;35;47;59] @>

  let xs = NonEmpty.assume [1;2;3;4;5]
  let ys = NonEmpty.assume [10;20;30;40;50;60]
  let zs = NonEmpty.assume [0 .. 100]

  let result = 
    List.NonEmpty.zipper {
      let! x = xs
      and! y = ys 
      and! z = zs 
      return x + y + z
    }

  test <@ result = NonEmpty.assume [11;23;35;47;59] @>
