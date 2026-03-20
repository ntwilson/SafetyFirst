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


module Splitting = 
  let toLists xs = Seq.map List.NonEmpty.toList xs |> List.ofSeq

  [<Test>]

  let ``returns what the documentation says`` () =

    test 
      <@
        (List.splitPairwise (=) [0;1;1;2;3;4;4;4;5] |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5]]
      @>

    test 
      <@
        (List.NonEmpty.split ((=) 100) (List.NonEmpty.create 1 [2;3;100;100;4;100;5;6]) |> toLists)
          = [[1;2;3;100];[100];[4;100];[5;6]]

        &&

        (List.NonEmpty.splitPairwise (=) (List.NonEmpty.create 0 [1;1;2;3;4;4;4;5]) |> toLists)
          = [[0;1];[1;2;3;4];[4];[4;5]]
      @>

  [<Test>]
  let ``splits empty and single element sequences`` () = 
    test 
      <@
        (List.splitPairwise (=) [] |> toLists) = []
        &&
        (List.splitPairwise (=) [0] |> toLists) = [[0]]
        &&
        (List.splitPairwise (=) [5; 5] |> toLists) = [[5]; [5]]
      @>
  
  [<Test>]
  let ``splits properly for multiple types of inputs`` () = 
    test 
      <@
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.singleton 0) |> toLists) = [[0]]
        &&
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.singleton 5) |> toLists) = [[5]]
        &&
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.create 0 [5]) |> toLists) = [[0; 5]]
        &&
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.create 5 [5]) |> toLists) = [[5]; [5]]
        &&
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.create 5 [0]) |> toLists) = [[5]; [0]]
        &&
        (List.NonEmpty.split ((=) 5) (List.NonEmpty.create 5 [0;0;5;5;0;5]) |> toLists) = [[5]; [0;0;5]; [5]; [0;5]]
      @>

  [<Test>]
  let ``splits pairwise properly for multiple types of inputs`` () = 
    let bigDiff i j = abs (i - j) > 5
    test 
      <@
        (List.NonEmpty.splitPairwise (=) (List.NonEmpty.singleton 0) |> toLists) = [[0]]
        &&
        (List.NonEmpty.splitPairwise (=) (List.NonEmpty.create 0 [1]) |> toLists) = [[0;1]]
        &&
        (List.NonEmpty.splitPairwise (=) (List.NonEmpty.create 0 [0]) |> toLists) = [[0]; [0]]
        &&
        (List.NonEmpty.splitPairwise (bigDiff) (List.NonEmpty.create 1 [2;12;13;23;24]) |> toLists)
          = [[1;2]; [12;13]; [23;24]]
        &&
        (List.NonEmpty.splitPairwise (bigDiff) (List.NonEmpty.create 1 [2;12;13;23]) |> toLists)
          = [[1;2]; [12;13]; [23]]
      @>

  [<Test>]
  let ``splitPairwise passes (previousElement, currentElement) to predicate`` () =
    // Because the list gets reversed, the logic around the order of arguments to the predicate is a little tricky.
    // A non-commutative predicate will produce wrong results if the arguments are flipped.
    let splitOnDescent (a: int) (b: int) = a > b  // split wherever the sequence descends
    test
      <@
        (List.splitPairwise splitOnDescent [1;3;5;2;4] |> toLists) = [[1;3;5]; [2;4]]
        &&
        (List.splitPairwise splitOnDescent [5;3;1;2;4] |> toLists) = [[5]; [3]; [1;2;4]]
        &&
        (List.NonEmpty.splitPairwise splitOnDescent (List.NonEmpty.create 1 [3;5;2;4]) |> toLists)
          = [[1;3;5]; [2;4]]
        &&
        (List.NonEmpty.splitPairwise splitOnDescent (List.NonEmpty.create 5 [3;1;2;4]) |> toLists)
          = [[5]; [3]; [1;2;4]]
      @>

  [<Test>]
  let ``split returns what the documentation says`` () =
    test
      <@
        (List.split ((=) 100) [1;2;3;100;100;4;100;5;6] |> toLists)
          = [[1;2;3;100];[100];[4;100];[5;6]]
      @>

  [<Test>]
  let ``works when the start and end elements match the predicate`` () =
    test 
      <@ 
        (List.split ((=) 100) [100; 100; 1; 2; 3; 100] |> toLists) 
          = [ [100]; [100]; [1;2;3;100] ] 
      @>

  [<Test>]
  let ``split handles empty and single element lists`` () =
    test
      <@
        (List.split ((=) 5) [] |> toLists) = []
        &&
        (List.split ((=) 5) [0] |> toLists) = [[0]]
        &&
        (List.split ((=) 5) [5] |> toLists) = [[5]]
        &&
        (List.split ((=) 5) [5;5] |> toLists) = [[5]; [5]]
      @>

  [<Test>]
  let ``split splits properly for multiple types of inputs`` () =
    test
      <@
        (List.split ((=) 5) [0] |> toLists) = [[0]]
        &&
        (List.split ((=) 5) [5] |> toLists) = [[5]]
        &&
        (List.split ((=) 5) [0;5] |> toLists) = [[0; 5]]
        &&
        (List.split ((=) 5) [5;5] |> toLists) = [[5]; [5]]
        &&
        (List.split ((=) 5) [5;0] |> toLists) = [[5]; [0]]
        &&
        (List.split ((=) 5) [5;0;0;5;5;0;5] |> toLists) = [[5]; [0;0;5]; [5]; [0;5]]
      @>

  [<Test>]
  let ``NonEmpty.takeUntilIncluding returns through the first matching element`` () =
    test
      <@
        List.NonEmpty.takeUntilIncluding ((=) 3) (List.NonEmpty.create 1 [2;3;4;5])
          = List.NonEmpty.create 1 [2;3]
        &&
        List.NonEmpty.takeUntilIncluding ((=) 1) (List.NonEmpty.create 1 [2;3;4;5])
          = List.NonEmpty.singleton 1
        &&
        List.NonEmpty.takeUntilIncluding ((=) 99) (List.NonEmpty.create 1 [2;3])
          = List.NonEmpty.create 1 [2;3]
      @>

module takeUntilIncluding =
  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ List.takeUntilIncluding (fun _ -> true) [] = [] @>

  [<Test>]
  let ``returns through the first matching element`` () =
    test <@ List.takeUntilIncluding ((=) 3) [1;2;3;4;5] = [1;2;3] @>

  [<Test>]
  let ``returns only the first element when it matches`` () =
    test <@ List.takeUntilIncluding ((=) 3) [3;4;5] = [3] @>

  [<Test>]
  let ``stops at the first match even when multiple elements match`` () =
    test <@ List.takeUntilIncluding ((=) 3) [1;3;3;3] = [1;3] @>

  [<Test>]
  let ``returns the full list when no element matches`` () =
    test <@ List.takeUntilIncluding ((=) 99) [1;2;3] = [1;2;3] @>

module SkipUntilIncluding =
  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ List.skipUntilIncluding (fun _ -> true) [] = [] @>

  [<Test>]
  let ``returns elements after the first matching element`` () =
    test <@ List.skipUntilIncluding ((=) 3) [1;2;3;4;5] = [4;5] @>

  [<Test>]
  let ``returns elements after the first element when it matches`` () =
    test <@ List.skipUntilIncluding ((=) 3) [3;4;5] = [4;5] @>

  [<Test>]
  let ``stops skipping at the first match even when multiple elements match`` () =
    test <@ List.skipUntilIncluding ((=) 3) [1;3;3;3] = [3;3] @>

  [<Test>]
  let ``returns empty when the match is the last element`` () =
    test <@ List.skipUntilIncluding ((=) 3) [1;2;3] = [] @>

  [<Test>]
  let ``returns empty when no element matches`` () =
    test <@ List.skipUntilIncluding ((=) 99) [1;2;3] = [] @>

  [<Test>]
  let ``takeUntilIncluding and skipUntilIncluding partition the list`` () =
    let xs = [1;2;3;4;5]
    let taken = List.takeUntilIncluding ((=) 3) xs
    let skipped = List.skipUntilIncluding ((=) 3) xs
    test <@ taken @ skipped = xs @>