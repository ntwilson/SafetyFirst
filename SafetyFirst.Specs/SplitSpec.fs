module SafetyFirst.Specs.SplitSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open ResultDotNet.FSharp

let toNonEmptySeq xs = FSeq.NonEmpty.ofFSeq' (fseq xs) |> Result.expect

let toSplitType (head, tail) = 
  (fseq head, fseq (Seq.map toNonEmptySeq tail))

[<Test>]
let ``can split an empty list`` () =
  test 
    <@
      FSeq.split ((=) 100) (fseq []) = toSplitType ([],[])
    @>

[<Test>]
let ``can split a single element that doesn't satisfy the predicate`` () =
  test
    <@
      FSeq.split ((=) 100) (fseq [1]) = toSplitType ([1],[])
    @>

[<Test>]
let ``can split a single element that does satisfy the predicate`` () = 
  test
    <@
      FSeq.split ((=) 100) (fseq [100]) = toSplitType ([],[[100]])
    @>

[<Test>]
let ``can split multiple elements satisfying the predicate in a row`` () =
  test
    <@
      FSeq.split ((=) 100) (fseq [100;100;100;1;2;100;100])
        = toSplitType ([], [[100];[100];[100;1;2];[100];[100]])
    @>

let toPairwiseType xs = 
  Seq.map toNonEmptySeq xs
  |> toNonEmptySeq

[<Test>]
let ``can split a sequence of 1 pairwise`` () =
  test 
    <@
      NonEmptySeq.splitPairwise (=) (toNonEmptySeq [1]) = 
        toPairwiseType [[1]]
    @>

[<Test>]
let ``can split a single pair that matches the predicate`` () =
  test
    <@
      NonEmptySeq.splitPairwise (=) (toNonEmptySeq [1;1]) =
        toPairwiseType [[1];[1]]
    @>

[<Test>]
let ``can split a single pair that doesn't match the predicate`` () =
  test
    <@
      NonEmptySeq.splitPairwise (=) (toNonEmptySeq [1;2]) =
        toPairwiseType [[1;2]]
    @>

[<Test>]
let ``can split on multiple different pairs`` () = 
  test
    <@
      NonEmptySeq.splitPairwise (=) (toNonEmptySeq [1;1;2;3;4;4;4;5;6;6]) =
        toPairwiseType [[1];[1;2;3;4];[4];[4;5;6];[6]]
    @>

let every4thValueIs100 = 
  InfiniteSeq.init (fun i -> if i % 4 = 0 then 100 else i)

[<Test>]
let ``can split infinite sequences`` () = 
  test
    <@ 
      let head, tail =     
        InfiniteSeq.split ((=) 100) (every4thValueIs100)

      head = fseq []
      &&
      InfiniteSeq.take 3 tail = 
        fseq [ 
          toNonEmptySeq [100;1;2;3]
          toNonEmptySeq [100;5;6;7]
          toNonEmptySeq [100;9;10;11]
        ]
    @>

[<Test>]
let ``can split infinite sequences pairwise`` () = 
  test
    <@ 
      let xs =
        InfiniteSeq.splitPairwise (fun a b -> abs (a - b) > 10) (every4thValueIs100)

      InfiniteSeq.take 7 xs = 
        fseq [ 
          toNonEmptySeq [100]
          toNonEmptySeq [1;2;3]
          toNonEmptySeq [100]
          toNonEmptySeq [5;6;7]
          toNonEmptySeq [100]
          toNonEmptySeq [9;10;11]
          toNonEmptySeq [100]
        ]
    @>
