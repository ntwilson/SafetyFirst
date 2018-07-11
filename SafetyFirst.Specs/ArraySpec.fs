module SafetyFirst.Specs.ArraySpec

open NUnit.Framework
open FsCheck

open SafetyFirst
open SafetyFirst.Specs.SeqSpec

let averageFloats' (xs:float[]) = Array.average' xs
let averageFloats (xs:float[]) = Array.average xs
let averageByFloats' (projection:_ -> float) xs = Array.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = Array.averageBy projection xs

[<Test>]
let ``Safe Seq functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrows1 averageFloats'          averageFloats
  errorsWheneverThrows2 averageByFloats'        averageByFloats
  errorsWheneverThrows2 Array.chunkBySize'      Array.chunkBySize
  errorsWheneverThrows1 Array.exactlyOne'       Array.exactlyOne
  errorsWheneverThrows2 Array.find'             Array.find
  errorsWheneverThrows2 Array.findBack'         Array.findBack
  errorsWheneverThrows2 Array.findIndex'        Array.findIndex
  errorsWheneverThrows2 Array.findIndexBack'    Array.findIndexBack
  errorsWheneverThrows4 Array.fold2'            Array.fold2
  errorsWheneverThrows4 Array.foldBack2'        Array.foldBack2
  errorsWheneverThrows3 Array.forall2'          Array.forall2
  errorsWheneverThrows1 Array.head'             Array.head
  errorsWheneverThrows2 Array.item'             Array.item
  errorsWheneverThrows1 Array.last'             Array.last
  errorsWheneverThrows3 Array.map2'             Array.map2
  errorsWheneverThrows3 Array.mapi2'            Array.mapi2
  errorsWheneverThrows4 Array.map3'             Array.map3
  errorsWheneverThrows1 Array.max'<int>         Array.max<int>
  errorsWheneverThrows2 Array.maxBy'<int, int>  Array.maxBy<int, int>
  errorsWheneverThrows1 Array.min'<int>         Array.min<int>
  errorsWheneverThrows2 Array.minBy'<int, int>  Array.minBy<int, int>
  errorsWheneverThrows2 Array.pick'             Array.pick
  errorsWheneverThrows2 Array.reduce'           Array.reduce
  errorsWheneverThrows2 Array.reduceBack'       Array.reduceBack
  errorsWheneverThrows2 Array.skip'             Array.skip
  errorsWheneverThrows2 Array.splitAt'          Array.splitAt
  errorsWheneverThrows2 Array.splitInto'        Array.splitInto
  errorsWheneverThrows3 Array.sub'              Array.sub
  errorsWheneverThrows1 Array.tail'             Array.tail
  errorsWheneverThrows2 Array.take'             Array.take
  errorsWheneverThrows2 Array.windowed'         Array.windowed
  errorsWheneverThrows2 Array.zip'              Array.zip
  errorsWheneverThrows3 Array.zip3'             Array.zip3
                                                

[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 averageFloats'         averageFloats
  alwaysProduceSameOutput2 averageByFloats'       averageByFloats
  alwaysProduceSameOutput2 Array.chunkBySize'     Array.chunkBySize
  alwaysProduceSameOutput1 Array.exactlyOne'      Array.exactlyOne
  alwaysProduceSameOutput2 Array.find'            Array.find
  alwaysProduceSameOutput2 Array.findBack'        Array.findBack
  alwaysProduceSameOutput2 Array.findIndex'       Array.findIndex
  alwaysProduceSameOutput2 Array.findIndexBack'   Array.findIndexBack
  alwaysProduceSameOutput4 Array.fold2'           Array.fold2
  alwaysProduceSameOutput4 Array.foldBack2'       Array.foldBack2
  alwaysProduceSameOutput3 Array.forall2'         Array.forall2
  alwaysProduceSameOutput1 Array.head'            Array.head
  alwaysProduceSameOutput2 Array.item'            Array.item
  alwaysProduceSameOutput1 Array.last'            Array.last
  alwaysProduceSameOutput3 Array.map2'            Array.map2
  alwaysProduceSameOutput3 Array.mapi2'           Array.mapi2
  alwaysProduceSameOutput4 Array.map3'            Array.map3
  alwaysProduceSameOutput1 Array.max'<int>        Array.max<int>
  alwaysProduceSameOutput2 Array.maxBy'<int, int> Array.maxBy<int, int>
  alwaysProduceSameOutput1 Array.min'<int>        Array.min<int>
  alwaysProduceSameOutput2 Array.minBy'<int, int> Array.minBy<int, int>
  alwaysProduceSameOutput2 Array.pick'            Array.pick
  alwaysProduceSameOutput2 Array.reduce'          Array.reduce
  alwaysProduceSameOutput2 Array.reduceBack'      Array.reduceBack
  alwaysProduceSameOutput2 Array.skip'            Array.skip
  alwaysProduceSameOutput2 Array.splitAt'         Array.splitAt
  alwaysProduceSameOutput2 Array.splitInto'       Array.splitInto
  alwaysProduceSameOutput3 Array.sub'             Array.sub      
  alwaysProduceSameOutput1 Array.tail'            Array.tail
  alwaysProduceSameOutput2 Array.take'            Array.take
  alwaysProduceSameOutput2 Array.windowed'        Array.windowed
  alwaysProduceSameOutput2 Array.zip'             Array.zip
  alwaysProduceSameOutput3 Array.zip3'            Array.zip3


