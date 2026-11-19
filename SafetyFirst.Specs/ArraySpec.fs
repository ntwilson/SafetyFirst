module SafetyFirst.Specs.ArraySpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec
open SafetyFirst.Specs.ResultFixtures
open SafetyFirst.Specs.OptionFixtures

let averageFloats' (xs:float[]) = Array.average' xs
let averageFloats (xs:float[]) = Array.average xs
let averageByFloats' (projection:_ -> float) xs = Array.averageBy' projection xs
let averageByFloats (projection:_ -> float) xs = Array.averageBy projection xs

[<Test>]
let ``Safe Array functions error whenever unsafe versions throw for all random inputs`` () =
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
  errorsWheneverThrowsForSeq1 Array.transpose'  Array.transpose
  errorsWheneverThrows2 Array.windowed'         Array.windowed
  errorsWheneverThrows2 Array.zip'              Array.zip
  errorsWheneverThrows3 Array.zip3'             Array.zip3


[<Test>]
let ``Safe Array functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 averageFloats'         averageFloats
  alwaysProduceSameOutput2 averageByFloats'       averageByFloats
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
  alwaysProduceSameOutput3 Array.sub'             Array.sub
  alwaysProduceSameOutput1 Array.tail'            Array.tail
  alwaysProduceSameOutput2 Array.take'            Array.take
  alwaysProduceSameOutputForSeq1 Array.transpose' Array.transpose
  alwaysProduceSameOutput2 Array.zip'             Array.zip
  alwaysProduceSameOutput3 Array.zip3'            Array.zip3


  alwaysProduceSameOutput2ExceptNonEmpty  Array.chunkBySize'  Array.chunkBySize
  alwaysProduceSameOutput2ExceptNonEmpty  Array.splitInto'    Array.splitInto
  alwaysProduceSameOutput2ExceptNonEmpty  Array.windowed'     Array.windowed

[<Test>]
let ``zips multiple arrays together via computation expression`` () =
  let xs = [|1;2;3;4;5|]
  let ys = [|10;20;30;40;50;60|]
  let zs = [|0 .. 100|]

  let result =
    Array.zipper {
      let! x = xs
      and! y = ys
      and! z = zs
      return x + y + z
    }

  test <@ result = [|11;23;35;47;59|] @>

  let xs = NonEmpty.assume [|1;2;3;4;5|]
  let ys = NonEmpty.assume [|10;20;30;40;50;60|]
  let zs = NonEmpty.assume [|0 .. 100|]

  let result =
    Array.NonEmpty.zipper {
      let! x = xs
      and! y = ys
      and! z = zs
      return x + y + z
    }

  test <@ result = NonEmpty.assume [|11;23;35;47;59|] @>

module Splitting =
  let toArrs xs = Seq.map Array.NonEmpty.toArray xs |> Array.ofSeq

  [<Test>]
  let ``returns what the documentation says`` () =

    test
      <@
        (Array.splitPairwise (=) [|0;1;1;2;3;4;4;4;5|] |> toArrs)
          = [|[|0;1|];[|1;2;3;4|];[|4|];[|4;5|]|]
      @>

    test
      <@
        (Array.NonEmpty.split ((=) 100) (Array.NonEmpty.create 1 [|2;3;100;100;4;100;5;6|]) |> toArrs)
          = [|[|1;2;3;100|];[|100|];[|4;100|];[|5;6|]|]

        &&

        (Array.NonEmpty.splitPairwise (=) (Array.NonEmpty.create 0 [|1;1;2;3;4;4;4;5|]) |> toArrs)
          = [|[|0;1|];[|1;2;3;4|];[|4|];[|4;5|]|]
      @>


  [<Test>]
  let ``splits empty and single element sequences`` () =
    test
      <@
        (Array.splitPairwise (=) [||] |> toArrs) = [||]
        &&
        (Array.splitPairwise (=) [|0|] |> toArrs) = [|[|0|]|]
        &&
        (Array.splitPairwise (=) [|5; 5|] |> toArrs) = [|[|5|]; [|5|]|]
      @>

  [<Test>]
  let ``splits properly for multiple types of inputs`` () =
    test
      <@
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.singleton 0) |> toArrs) = [|[|0|]|]
        &&
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.singleton 5) |> toArrs) = [|[|5|]|]
        &&
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.create 0 [|5|]) |> toArrs) = [|[|0; 5|]|]
        &&
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.create 5 [|5|]) |> toArrs) = [|[|5|]; [|5|]|]
        &&
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.create 5 [|0|]) |> toArrs) = [|[|5|]; [|0|]|]
        &&
        (Array.NonEmpty.split ((=) 5) (Array.NonEmpty.create 5 [|0;0;5;5;0;5|]) |> toArrs) = [|[|5|]; [|0;0;5|]; [|5|]; [|0;5|]|]
      @>

  [<Test>]
  let ``splits pairwise properly for multiple types of inputs`` () =
    let bigDiff i j = abs (i - j) > 5
    test
      <@
        (Array.NonEmpty.splitPairwise (=) (Array.NonEmpty.singleton 0) |> toArrs) = [|[|0|]|]
        &&
        (Array.NonEmpty.splitPairwise (=) (Array.NonEmpty.create 0 [|1|]) |> toArrs) = [|[|0;1|]|]
        &&
        (Array.NonEmpty.splitPairwise (=) (Array.NonEmpty.create 0 [|0|]) |> toArrs) = [|[|0|]; [|0|]|]
        &&
        (Array.NonEmpty.splitPairwise (bigDiff) (Array.NonEmpty.create 1 [|2;12;13;23;24|]) |> toArrs)
          = [|[|1;2|]; [|12;13|]; [|23;24|]|]
        &&
        (Array.NonEmpty.splitPairwise (bigDiff) (Array.NonEmpty.create 1 [|2;12;13;23|]) |> toArrs)
          = [|[|1;2|]; [|12;13|]; [|23|]|]
      @>

  [<Test>]
  let ``split returns what the documentation says`` () =
    test
      <@
        (Array.split ((=) 100) [|1;2;3;100;100;4;100;5;6|] |> toArrs)
          = [|[|1;2;3;100|];[|100|];[|4;100|];[|5;6|]|]
      @>

  [<Test>]
  let ``split handles empty and single element arrays`` () =
    test
      <@
        (Array.split ((=) 5) [||] |> toArrs) = [||]
        &&
        (Array.split ((=) 5) [|0|] |> toArrs) = [|[|0|]|]
        &&
        (Array.split ((=) 5) [|5|] |> toArrs) = [|[|5|]|]
        &&
        (Array.split ((=) 5) [|5;5|] |> toArrs) = [|[|5|]; [|5|]|]
      @>

  [<Test>]
  let ``split splits properly for multiple types of inputs`` () =
    test
      <@
        (Array.split ((=) 5) [|0|] |> toArrs) = [|[|0|]|]
        &&
        (Array.split ((=) 5) [|5|] |> toArrs) = [|[|5|]|]
        &&
        (Array.split ((=) 5) [|0;5|] |> toArrs) = [|[|0; 5|]|]
        &&
        (Array.split ((=) 5) [|5;5|] |> toArrs) = [|[|5|]; [|5|]|]
        &&
        (Array.split ((=) 5) [|5;0|] |> toArrs) = [|[|5|]; [|0|]|]
        &&
        (Array.split ((=) 5) [|5;0;0;5;5;0;5|] |> toArrs) = [|[|5|]; [|0;0;5|]; [|5|]; [|0;5|]|]
      @>

module TakeUntilIncluding =
  [<Test>]
  let ``NonEmpty.takeUntilIncluding returns through the first matching element`` () =
    test
      <@
        Array.NonEmpty.takeUntilIncluding ((=) 3) (Array.NonEmpty.create 1 [|2;3;4;5|])
          = Array.NonEmpty.create 1 [|2;3|]
        &&
        Array.NonEmpty.takeUntilIncluding ((=) 1) (Array.NonEmpty.create 1 [|2;3;4;5|])
          = Array.NonEmpty.singleton 1
        &&
        Array.NonEmpty.takeUntilIncluding ((=) 99) (Array.NonEmpty.create 1 [|2;3|])
          = Array.NonEmpty.create 1 [|2;3|]
      @>

  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ Array.takeUntilIncluding (fun _ -> true) [||] = [||] @>

  [<Test>]
  let ``returns through the first matching element`` () =
    test <@ Array.takeUntilIncluding ((=) 3) [|1;2;3;4;5|] = [|1;2;3|] @>

  [<Test>]
  let ``returns only the first element when it matches`` () =
    test <@ Array.takeUntilIncluding ((=) 3) [|3;4;5|] = [|3|] @>

  [<Test>]
  let ``stops at the first match even when multiple elements match`` () =
    test <@ Array.takeUntilIncluding ((=) 3) [|1;3;3;3|] = [|1;3|] @>

  [<Test>]
  let ``returns the full array when no element matches`` () =
    test <@ Array.takeUntilIncluding ((=) 99) [|1;2;3|] = [|1;2;3|] @>

module SkipUntilIncluding =
  [<Test>]
  let ``returns empty for empty input`` () =
    test <@ Array.skipUntilIncluding (fun _ -> true) [||] = [||] @>

  [<Test>]
  let ``returns elements after the first matching element`` () =
    test <@ Array.skipUntilIncluding ((=) 3) [|1;2;3;4;5|] = [|4;5|] @>

  [<Test>]
  let ``returns elements after the first element when it matches`` () =
    test <@ Array.skipUntilIncluding ((=) 3) [|3;4;5|] = [|4;5|] @>

  [<Test>]
  let ``stops skipping at the first match even when multiple elements match`` () =
    test <@ Array.skipUntilIncluding ((=) 3) [|1;3;3;3|] = [|3;3|] @>

  [<Test>]
  let ``returns empty when the match is the last element`` () =
    test <@ Array.skipUntilIncluding ((=) 3) [|1;2;3|] = [||] @>

  [<Test>]
  let ``returns empty when no element matches`` () =
    test <@ Array.skipUntilIncluding ((=) 99) [|1;2;3|] = [||] @>

  [<Test>]
  let ``takeUntilIncluding and skipUntilIncluding partition the array`` () =
    let xs = [|1;2;3;4;5|]
    let taken = Array.takeUntilIncluding ((=) 3) xs
    let skipped = Array.skipUntilIncluding ((=) 3) xs
    test <@ Array.append taken skipped = xs @>

[<Test>]
let ``Array.Result functions`` () =
  let okAndErr = List.toArray okAndErr
  let allOk = List.toArray allOk

  test <@ Array.Result.traverse tenfoldUnlessTwo [|1; 1|] = Ok [|10; 10|] @>
  test <@ Array.Result.traverse tenfoldUnlessTwo [|1; 2; 3|] = Error "two" @>
  test <@ Array.Result.traverse tenfoldUnlessTwo [||] = Ok [||] @>
  test <@ Array.Result.sequence okAndErr = Error "a" @>
  test <@ Array.Result.sequence allOk = Ok [|1; 2|] @>
  test <@ Array.Result.collect okAndErr = Error (NonEmpty.assume [|"a"; "b"|]) @>
  test <@ Array.Result.collect allOk = Ok [|1; 2|] @>
  test <@ Array.Result.partitionMap tenfoldIfEven [|1; 2; 3; 4|] = ([|20; 40|], [|"1 is odd"; "3 is odd"|]) @>
  test <@ Array.Result.partition okAndErr = ([|1; 2|], [|"a"; "b"|]) @>

[<Test>]
let ``Array.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (List.toArray okAndErr)
  let allOk = NonEmpty.assume (List.toArray allOk)

  test <@ Array.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [|1; 1|]) = Ok (NonEmpty.assume [|10; 10|]) @>
  test <@ Array.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [|1; 2; 3|]) = Error "two" @>
  test <@ Array.NonEmpty.Result.sequence okAndErr = Error "a" @>
  test <@ Array.NonEmpty.Result.sequence allOk = Ok (NonEmpty.assume [|1; 2|]) @>
  test <@ Array.NonEmpty.Result.collect okAndErr = Error (NonEmpty.assume [|"a"; "b"|]) @>
  test <@ Array.NonEmpty.Result.collect allOk = Ok (NonEmpty.assume [|1; 2|]) @>
  test <@ Array.NonEmpty.Result.partitionMap tenfoldIfEven (NonEmpty.assume [|1; 2; 3; 4|]) = ([|20; 40|], [|"1 is odd"; "3 is odd"|]) @>
  test <@ Array.NonEmpty.Result.partition okAndErr = ([|1; 2|], [|"a"; "b"|]) @>

[<Test>]
let ``Array.Option functions`` () =
  test <@ Array.Option.traverse tenfoldUnlessTwoOpt [|1; 1|] = Some [|10; 10|] @>
  test <@ Array.Option.traverse tenfoldUnlessTwoOpt [|1; 2; 3|] = None @>
  test <@ Array.Option.traverse tenfoldUnlessTwoOpt [||] = Some [||] @>
  test <@ Array.Option.sequence (List.toArray someAndNone) = None @>
  test <@ Array.Option.sequence (List.toArray allSome) = Some [|1; 2|] @>

[<Test>]
let ``Array.NonEmpty.Option functions`` () =
  test <@ Array.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume [|1; 1|]) = Some (NonEmpty.assume [|10; 10|]) @>
  test <@ Array.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume [|1; 2; 3|]) = None @>
  test <@ Array.NonEmpty.Option.sequence (NonEmpty.assume (List.toArray someAndNone)) = None @>
  test <@ Array.NonEmpty.Option.sequence (NonEmpty.assume (List.toArray allSome)) = Some (NonEmpty.assume [|1; 2|]) @>

[<Test>]
let ``Array.ValueOption functions`` () =
  test <@ Array.ValueOption.traverse tenfoldUnlessTwoVOpt [|1; 1|] = ValueSome [|10; 10|] @>
  test <@ Array.ValueOption.traverse tenfoldUnlessTwoVOpt [|1; 2; 3|] = ValueNone @>
  test <@ Array.ValueOption.traverse tenfoldUnlessTwoVOpt [||] = ValueSome [||] @>
  test <@ Array.ValueOption.sequence (List.toArray vsomeAndNone) = ValueNone @>
  test <@ Array.ValueOption.sequence (List.toArray allVSome) = ValueSome [|1; 2|] @>

[<Test>]
let ``Array.NonEmpty.ValueOption functions`` () =
  test <@ Array.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume [|1; 1|]) = ValueSome (NonEmpty.assume [|10; 10|]) @>
  test <@ Array.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume [|1; 2; 3|]) = ValueNone @>
  test <@ Array.NonEmpty.ValueOption.sequence (NonEmpty.assume (List.toArray vsomeAndNone)) = ValueNone @>
  test <@ Array.NonEmpty.ValueOption.sequence (NonEmpty.assume (List.toArray allVSome)) = ValueSome (NonEmpty.assume [|1; 2|]) @>

module NonEmptyParity =
  open SafetyFirst.Numbers

  let sameOrBothFail1 safeVersion unsafeVersion = nonEmptyMatchesThrowing1 Array.ofList Array.ofList safeVersion unsafeVersion
  let sameOrBothFail2 safeVersion unsafeVersion = nonEmptyMatchesThrowing2 Array.ofList Array.ofList safeVersion unsafeVersion
  let sameOrBothFailZip2 safeVersion unsafeVersion = nonEmptyMatchesThrowingZip2 Array.ofList Array.ofList safeVersion unsafeVersion
  let sameOrBothFailZip3 safeVersion unsafeVersion = nonEmptyMatchesThrowingZip3 Array.ofList Array.ofList safeVersion unsafeVersion
  let same1 nonEmptyVersion plainVersion = nonEmptyMatches1 Array.ofList Array.ofList nonEmptyVersion plainVersion
  let same2 nonEmptyVersion plainVersion = nonEmptyMatches2 Array.ofList Array.ofList nonEmptyVersion plainVersion

  [<Test>]
  let ``Array.NonEmpty functions error whenever the Array versions throw, and otherwise match them`` () =
    sameOrBothFail2 Array.NonEmpty.chunkBySize'                                       Array.chunkBySize
    sameOrBothFail2 (fun n -> Array.NonEmpty.tryChunkBySize n >> Option.toResult ())  Array.chunkBySize
    sameOrBothFail1 Array.NonEmpty.exactlyOne'                                        Array.exactlyOne
    sameOrBothFail1 (Array.NonEmpty.tryExactlyOne >> Option.toResult ())              Array.exactlyOne
    sameOrBothFail2 Array.NonEmpty.item'                                              Array.item
    sameOrBothFail2 Array.NonEmpty.pick'                                              Array.pick
    sameOrBothFail2 Array.NonEmpty.splitAt'                                           Array.splitAt
    sameOrBothFail2 (fun n -> Array.NonEmpty.trySplitAt n >> Option.toResult ())      Array.splitAt
    sameOrBothFail2 Array.NonEmpty.splitInto'                                         Array.splitInto
    sameOrBothFail2 (fun n -> Array.NonEmpty.trySplitInto n >> Option.toResult ())    Array.splitInto
    sameOrBothFail2 (fun (i, n) xs -> Array.NonEmpty.sub' xs i n)                     (fun (i, n) xs -> Array.sub xs i n)
    sameOrBothFail2 (fun (i, n) xs -> Array.NonEmpty.trySub xs i n |> Option.toResult ()) (fun (i, n) xs -> Array.sub xs i n)

    sameOrBothFailZip2 (fun (f, s) -> Array.NonEmpty.fold2' f s)                     (fun (f, s) -> Array.fold2 f s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> Array.NonEmpty.tryFold2 f s xs ys |> Option.toResult ()) (fun (f, s) -> Array.fold2 f s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> Array.NonEmpty.foldBack2' f xs ys s)      (fun (f, s) xs ys -> Array.foldBack2 f xs ys s)
    sameOrBothFailZip2 (fun (f, s) xs ys -> Array.NonEmpty.tryFoldBack2 f xs ys s |> Option.toResult ()) (fun (f, s) xs ys -> Array.foldBack2 f xs ys s)
    sameOrBothFailZip2 Array.NonEmpty.forall2'                                        Array.forall2
    sameOrBothFailZip2 (fun f xs ys -> Array.NonEmpty.tryForall2 f xs ys |> Option.toResult ()) Array.forall2
    sameOrBothFailZip2 Array.NonEmpty.mapi2'                                          Array.mapi2
    sameOrBothFailZip2 (fun f xs ys -> Array.NonEmpty.tryMapi2 f xs ys |> Option.toResult ()) Array.mapi2
    sameOrBothFailZip3 Array.NonEmpty.map3'                                           Array.map3
    sameOrBothFailZip3 (fun f xs ys zs -> Array.NonEmpty.tryMap3 f xs ys zs |> Option.toResult ()) Array.map3

    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> Array.NonEmpty.iter2' (fun x y -> visit (x, y)) xs ys))
      (fun () xs ys -> visits (fun visit -> Array.iter2 (fun x y -> visit (x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> Array.NonEmpty.tryIter2 (fun x y -> visit (x, y)) xs ys |> Option.toResult ()))
      (fun () xs ys -> visits (fun visit -> Array.iter2 (fun x y -> visit (x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> Array.NonEmpty.iteri2' (fun i x y -> visit (i, x, y)) xs ys))
      (fun () xs ys -> visits (fun visit -> Array.iteri2 (fun i x y -> visit (i, x, y)) xs ys))
    sameOrBothFailZip2
      (fun () xs ys -> visitsOrError (fun visit -> Array.NonEmpty.tryIteri2 (fun i x y -> visit (i, x, y)) xs ys |> Option.toResult ()))
      (fun () xs ys -> visits (fun visit -> Array.iteri2 (fun i x y -> visit (i, x, y)) xs ys))

  [<Test>]
  let ``Array.NonEmpty functions always match the Array versions`` () =
    same2 (fun n -> Array.NonEmpty.chunksOf (PositiveInt.assume (toPositive n)))    (fun n -> Array.chunkBySize (toPositive n))
    same2 (fun n -> Array.NonEmpty.splitIntoN (PositiveInt.assume (toPositive n)))  (fun n -> Array.splitInto (toPositive n))
    same2 Array.NonEmpty.tryPick                                                    Array.tryPick
    same2 Array.NonEmpty.skipUntilIncluding                                         Array.skipUntilIncluding
    same1 (fun xs -> (Array.NonEmpty.sum xs : int))                                 Array.sum<int>
    same2 (fun (f: _ -> int) -> Array.NonEmpty.sumBy f)                             (fun f -> Array.sumBy f)
    same2 Array.NonEmpty.choose                                                     Array.choose
    same2 Array.NonEmpty.takeWhile                                                  Array.takeWhile
    same2 Array.NonEmpty.skipWhile                                                  Array.skipWhile
    same2 Array.NonEmpty.sortWith                                                   Array.sortWith
    same2 (fun (f, s) xs -> Array.NonEmpty.scanBack f xs s)                         (fun (f, s) xs -> Array.scanBack f xs s)
    same2 (fun (f, s) -> Array.NonEmpty.mapFold f s)                                (fun (f, s) -> Array.mapFold f s)
    same2 (fun (f, s) xs -> Array.NonEmpty.mapFoldBack f xs s)                      (fun (f, s) xs -> Array.mapFoldBack f xs s)
    same1 (fun xs -> visits (fun visit -> Array.NonEmpty.iter visit xs))           (fun xs -> visits (fun visit -> Array.iter visit xs))
    same1
      (fun xs -> visits (fun visit -> Array.NonEmpty.iteri (fun i x -> visit (i, x)) xs))
      (fun xs -> visits (fun visit -> Array.iteri (fun i x -> visit (i, x)) xs))
