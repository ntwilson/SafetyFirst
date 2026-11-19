module SafetyFirst.Specs.ListSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec
open SafetyFirst.Specs.ResultFixtures
open SafetyFirst.Specs.OptionFixtures

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

module TakeUntilIncluding =
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

[<Test>]
let ``List.Result functions`` () =
  test <@ List.Result.traverse tenfoldUnlessTwo [1; 1] = Ok [10; 10] @>
  test <@ List.Result.traverse tenfoldUnlessTwo [1; 2; 3] = Error "two" @>
  test <@ List.Result.traverse tenfoldUnlessTwo [] = Ok [] @>
  test <@ List.Result.sequence okAndErr = Error "a" @>
  test <@ List.Result.sequence allOk = Ok [1; 2] @>
  test <@ List.Result.collect okAndErr = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ List.Result.collect allOk = Ok [1; 2] @>
  test <@ List.Result.partitionMap tenfoldIfEven [1; 2; 3; 4] = ([20; 40], ["1 is odd"; "3 is odd"]) @>
  test <@ List.Result.partition okAndErr = ([1; 2], ["a"; "b"]) @>

[<Test>]
let ``List.NonEmpty.Result functions`` () =
  test <@ List.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [1; 1]) = Ok (NonEmpty.assume [10; 10]) @>
  test <@ List.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [1; 2; 3]) = Error "two" @>
  test <@ List.NonEmpty.Result.sequence (NonEmpty.assume okAndErr) = Error "a" @>
  test <@ List.NonEmpty.Result.sequence (NonEmpty.assume allOk) = Ok (NonEmpty.assume [1; 2]) @>
  test <@ List.NonEmpty.Result.collect (NonEmpty.assume okAndErr) = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ List.NonEmpty.Result.collect (NonEmpty.assume allOk) = Ok (NonEmpty.assume [1; 2]) @>
  test <@ List.NonEmpty.Result.partitionMap tenfoldIfEven (NonEmpty.assume [1; 2; 3; 4]) = ([20; 40], ["1 is odd"; "3 is odd"]) @>
  test <@ List.NonEmpty.Result.partition (NonEmpty.assume okAndErr) = ([1; 2], ["a"; "b"]) @>

[<Test>]
let ``List.Option functions`` () =
  test <@ List.Option.traverse tenfoldUnlessTwoOpt [1; 1] = Some [10; 10] @>
  test <@ List.Option.traverse tenfoldUnlessTwoOpt [1; 2; 3] = None @>
  test <@ List.Option.traverse tenfoldUnlessTwoOpt [] = Some [] @>
  test <@ List.Option.sequence someAndNone = None @>
  test <@ List.Option.sequence allSome = Some [1; 2] @>

[<Test>]
let ``List.NonEmpty.Option functions`` () =
  test <@ List.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume [1; 1]) = Some (NonEmpty.assume [10; 10]) @>
  test <@ List.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume [1; 2; 3]) = None @>
  test <@ List.NonEmpty.Option.sequence (NonEmpty.assume someAndNone) = None @>
  test <@ List.NonEmpty.Option.sequence (NonEmpty.assume allSome) = Some (NonEmpty.assume [1; 2]) @>

[<Test>]
let ``List.ValueOption functions`` () =
  test <@ List.ValueOption.traverse tenfoldUnlessTwoVOpt [1; 1] = ValueSome [10; 10] @>
  test <@ List.ValueOption.traverse tenfoldUnlessTwoVOpt [1; 2; 3] = ValueNone @>
  test <@ List.ValueOption.traverse tenfoldUnlessTwoVOpt [] = ValueSome [] @>
  test <@ List.ValueOption.sequence vsomeAndNone = ValueNone @>
  test <@ List.ValueOption.sequence allVSome = ValueSome [1; 2] @>

[<Test>]
let ``List.NonEmpty.ValueOption functions`` () =
  test <@ List.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume [1; 1]) = ValueSome (NonEmpty.assume [10; 10]) @>
  test <@ List.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume [1; 2; 3]) = ValueNone @>
  test <@ List.NonEmpty.ValueOption.sequence (NonEmpty.assume vsomeAndNone) = ValueNone @>
  test <@ List.NonEmpty.ValueOption.sequence (NonEmpty.assume allVSome) = ValueSome (NonEmpty.assume [1; 2]) @>

module NonEmptyParity =
  open SafetyFirst.Numbers

  let sameOrBothFail1 safeVersion unsafeVersion = nonEmptyMatchesThrowing1 id id safeVersion unsafeVersion
  let sameOrBothFail2 safeVersion unsafeVersion = nonEmptyMatchesThrowing2 id id safeVersion unsafeVersion
  let sameOrBothFailZip2 safeVersion unsafeVersion = nonEmptyMatchesThrowingZip2 id id safeVersion unsafeVersion
  let sameOrBothFailZip3 safeVersion unsafeVersion = nonEmptyMatchesThrowingZip3 id id safeVersion unsafeVersion
  let same1 nonEmptyVersion plainVersion = nonEmptyMatches1 id id nonEmptyVersion plainVersion
  let same2 nonEmptyVersion plainVersion = nonEmptyMatches2 id id nonEmptyVersion plainVersion

  [<Test>]
  let ``List.NonEmpty functions error whenever the List versions throw, and otherwise match them`` () =
    sameOrBothFail2 List.NonEmpty.chunkBySize'                                       List.chunkBySize
    sameOrBothFail2 (fun n -> List.NonEmpty.tryChunkBySize n >> Option.toResult ())  List.chunkBySize
    sameOrBothFail1 List.NonEmpty.exactlyOne'                                        List.exactlyOne
    sameOrBothFail1 (List.NonEmpty.tryExactlyOne >> Option.toResult ())              List.exactlyOne
    sameOrBothFail2 List.NonEmpty.item'                                              List.item
    sameOrBothFail2 List.NonEmpty.pick'                                              List.pick
    sameOrBothFail2 List.NonEmpty.splitAt'                                           List.splitAt
    sameOrBothFail2 (fun n -> List.NonEmpty.trySplitAt n >> Option.toResult ())      List.splitAt
    sameOrBothFail2 List.NonEmpty.splitInto'                                         List.splitInto
    sameOrBothFail2 (fun n -> List.NonEmpty.trySplitInto n >> Option.toResult ())    List.splitInto

    sameOrBothFailZip2 (fun (f, s) -> List.NonEmpty.fold2' f s)                     (fun (f, s) -> List.fold2 f s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> List.NonEmpty.tryFold2 f s xs ys |> Option.toResult ()) (fun (f, s) -> List.fold2 f s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> List.NonEmpty.foldBack2' f xs ys s)      (fun (f, s) xs ys -> List.foldBack2 f xs ys s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> List.NonEmpty.tryFoldBack2 f xs ys s |> Option.toResult ()) (fun (f, s) xs ys -> List.foldBack2 f xs ys s)
    sameOrBothFailZip2 List.NonEmpty.forall2'                                        List.forall2
    sameOrBothFailZip2 (fun f xs ys -> List.NonEmpty.tryForall2 f xs ys |> Option.toResult ()) List.forall2
    sameOrBothFailZip2 List.NonEmpty.mapi2'                                          List.mapi2
    sameOrBothFailZip2 (fun f xs ys -> List.NonEmpty.tryMapi2 f xs ys |> Option.toResult ()) List.mapi2
    sameOrBothFailZip3 List.NonEmpty.map3'                                           List.map3
    sameOrBothFailZip3 (fun f xs ys zs -> List.NonEmpty.tryMap3 f xs ys zs |> Option.toResult ()) List.map3

    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> List.NonEmpty.iter2' (fun x y -> visit (x, y)) xs ys))
      (fun () xs ys -> visits (fun visit -> List.iter2 (fun x y -> visit (x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> List.NonEmpty.tryIter2 (fun x y -> visit (x, y)) xs ys |> Option.toResult ()))
      (fun () xs ys -> visits (fun visit -> List.iter2 (fun x y -> visit (x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> List.NonEmpty.iteri2' (fun i x y -> visit (i, x, y)) xs ys))
      (fun () xs ys -> visits (fun visit -> List.iteri2 (fun i x y -> visit (i, x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> List.NonEmpty.tryIteri2 (fun i x y -> visit (i, x, y)) xs ys |> Option.toResult ()))
      (fun () xs ys -> visits (fun visit -> List.iteri2 (fun i x y -> visit (i, x, y)) xs ys))

  [<Test>]
  let ``List.NonEmpty functions always match the List versions`` () =
    same2 (fun n -> List.NonEmpty.chunksOf (PositiveInt.assume (toPositive n)))    (fun n -> List.chunkBySize (toPositive n))
    same2 (fun n -> List.NonEmpty.splitIntoN (PositiveInt.assume (toPositive n)))  (fun n -> List.splitInto (toPositive n))
    same2 List.NonEmpty.tryPick                                                    List.tryPick
    same2 List.NonEmpty.skipUntilIncluding                                         List.skipUntilIncluding
    same1 (fun xs -> (List.NonEmpty.sum xs : int))                                 List.sum<int>
    same2 (fun (f: _ -> int) -> List.NonEmpty.sumBy f)                             (fun f -> List.sumBy f)
    same2 List.NonEmpty.choose                                                     List.choose
    same2 List.NonEmpty.takeWhile                                                  List.takeWhile
    same2 List.NonEmpty.skipWhile                                                  List.skipWhile
    same2 List.NonEmpty.sortWith                                                   List.sortWith
    same2 (fun (f, s) xs -> List.NonEmpty.scanBack f xs s)                         (fun (f, s) xs -> List.scanBack f xs s)
    same2 (fun (f, s) -> List.NonEmpty.mapFold f s)                                (fun (f, s) -> List.mapFold f s)
    same2 (fun (f, s) xs -> List.NonEmpty.mapFoldBack f xs s)                      (fun (f, s) xs -> List.mapFoldBack f xs s)
    same1 (fun xs -> visits (fun visit -> List.NonEmpty.iter visit xs))           (fun xs -> visits (fun visit -> List.iter visit xs))
    same1
      (fun xs -> visits (fun visit -> List.NonEmpty.iteri (fun i x -> visit (i, x)) xs))
      (fun xs -> visits (fun visit -> List.iteri (fun i x -> visit (i, x)) xs))
