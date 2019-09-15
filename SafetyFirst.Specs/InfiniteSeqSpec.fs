module SafetyFirst.Specs.InfiniteSeqSpec

// #load "./deps.fsx"
// #load "../SafetyFirst/InfiniteSeq.fsx"

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Numbers

let always x _ = x
let wellFormedList = InfiniteSeq.init (MaxElements 10000) id
let illFormedList = InfiniteSeq.init (MaxElements 1000) (always 0) |> InfiniteSeq.filter ((<>) 0)
let hung = InfiniteSeqHung "Program execution hung.  This infinite sequence was allowed to evaluate elements for too long."

let take n xs = InfiniteSeq.take' n xs |> Result.map Seq.toList


[<Test>]
let ``grabbing an item at a given index does not hang`` () = 
  test
    <@
      wellFormedList |> InfiniteSeq.item' (NaturalInt.assume 100) = Ok 100
      &&
      illFormedList |> InfiniteSeq.item' (NaturalInt.assume 100) |> Result.isError
    @>

[<Test>]
let ``eagerly taking elements does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.take' 5 |> Result.map Seq.toList = Ok [0..4]
      &&
      illFormedList |> InfiniteSeq.take' 5 |> Result.isError
    @>

[<Test>]
let ``lazily taking elements does not hang`` () = 
  let listWith5 = InfiniteSeq.init (MaxElements 1000) (fun i -> i - 5) |> InfiniteSeq.filter (fun i -> i < 0)

  test 
    <@
      listWith5 |> InfiniteSeq.truncate 4 |> Seq.toList = ([-5 .. -2] |> List.map Ok)
      &&
      listWith5 |> InfiniteSeq.truncate 7 |> Seq.toList = ([-5 .. -1] |> List.map Ok) @ [Error hung]
    @>

[<Test>]
let ``eagerly taking while some predicate does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.takeWhile' (fun i -> i < 10) |> Result.map Seq.toList = Ok [0 .. 9]
      &&
      illFormedList |> InfiniteSeq.takeWhile' (fun i -> i < 10) |> Result.isError
    @>

[<Test>]
let ``lazily taking while some predicate can return an infinite result and does not hang`` () =
  test 
    <@
      wellFormedList |> InfiniteSeq.takeWhileLazy (always true) |> Seq.take 10 |> Seq.toList = ([0 .. 9] |> List.map Ok)
      &&
      illFormedList |> InfiniteSeq.takeWhileLazy (always true) |> Seq.toList = [Error hung]
      &&
      wellFormedList |> InfiniteSeq.takeWhileLazy (fun i -> i < 10) |> Seq.toList = ([0 .. 9] |> List.map Ok)
    @>    

[<Test>]
let ``choosing by a function returning an option does not hang`` () = 
  let even i =  
    if i % 2 = 0 
    then Some i
    else None

  test 
    <@
      wellFormedList |> InfiniteSeq.choose even |> take 3 = Ok [0;2;4]
      &&
      illFormedList |> InfiniteSeq.choose even |> take 3 |> Result.isError
    @>

[<Test>]
let ``chunking by some finite size does not hang`` () = 
  let listWith5 = InfiniteSeq.init (MaxElements 1000) (fun i -> i - 4) |> InfiniteSeq.filter (fun i -> i <= 0)
  test
    <@
      wellFormedList |> InfiniteSeq.chunkBySizeUnsafe 2 |> take 3
        = Ok [ [|0;1|]; [|2;3|]; [|4;5|] ]
      &&
      illFormedList |> InfiniteSeq.chunkBySizeUnsafe 2 |> take 3 |> Result.isError
      &&
      listWith5 |> InfiniteSeq.chunkBySizeUnsafe 2 |> take 2 = Ok [ [|-4;-3|]; [|-2;-1|] ]
      &&
      listWith5 |> InfiniteSeq.chunkBySizeUnsafe 2 |> take 3 |> Result.isError
    @>

[<Test>]
let ``skipping does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.skip 5 |> take 5 = Ok [5 .. 9]
      &&
      illFormedList |> InfiniteSeq.skip 5 |> take 5 |> Result.isError 
      &&
      wellFormedList |> InfiniteSeq.skipWhile (fun i -> i < 10) |> take 5 = Ok [10 .. 14]
      &&
      illFormedList |> InfiniteSeq.skipWhile (fun i -> i < 10) |> take 5 |> Result.isError
      &&
      wellFormedList |> InfiniteSeq.skipWhile (always true) |> take 1 |> Result.isError
    @>

[<Test>]
let ``taking the head does not hang`` () =
  test 
    <@
      wellFormedList |> InfiniteSeq.head' = Ok 0
      &&
      illFormedList |> InfiniteSeq.head' |> Result.isError
    @>    

[<Test>]
let ``taking the tail does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.tail' |> Result.map (take 5) = Ok (Ok [1 .. 5])
      &&
      illFormedList |> InfiniteSeq.tail' |> Result.isError
    @>

[<Test>]
let ``unconsing does not hang`` () = 
  test 
    <@
      match InfiniteSeq.uncons' wellFormedList with
      | Ok (head, tail) -> head = 0 && take 3 tail = Ok [1 .. 3]
      | Error _ -> false
      &&
      InfiniteSeq.uncons' illFormedList |> Result.isError
      &&
      InfiniteSeq.filter (fun i -> i = 0) wellFormedList |> InfiniteSeq.uncons' |> Result.isOk
    @>

[<Test>]
let ``mapping does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.map ((+) 5) |> take 5 = Ok [5 .. 9]
      &&
      illFormedList |> InfiniteSeq.map ((+) 5) |> InfiniteSeq.head' |> Result.isError
    @>

[<Test>]
let ``mapping with index does not hang`` () = 
  test 
    <@
      wellFormedList |> InfiniteSeq.mapi (+) |> take 5 = Ok [0; 2; 4; 6; 8]
      &&
      illFormedList |> InfiniteSeq.mapi (+) |> InfiniteSeq.head' |> Result.isError
    @>

[<Test>]
let ``mapping two infinite sequences does not hang`` () =
  test 
    <@
      InfiniteSeq.map2 (+) wellFormedList illFormedList |> InfiniteSeq.head' |> Result.isError
      &&
      InfiniteSeq.map2 (+) illFormedList wellFormedList |> InfiniteSeq.head' |> Result.isError
      &&
      InfiniteSeq.map2 (+) wellFormedList wellFormedList |> take 5 = Ok [0;2;4;6;8]
      &&
      InfiniteSeq.map2 (+) illFormedList illFormedList |> InfiniteSeq.head' |> Result.isError
    @>

[<Test>]
let ``mapping an infinite seq and a finite seq does not hang`` () = 
  test 
    <@
      InfiniteSeq.map2L (+) wellFormedList (fseq [1 .. 4]) = Ok (fseq [1;3;5;7])
      &&
      InfiniteSeq.map2L (+) illFormedList (fseq [1 .. 4]) |> Result.isError
      &&
      InfiniteSeq.map2R (+) (fseq [1 .. 4]) wellFormedList = Ok (fseq [1;3;5;7])
      &&
      InfiniteSeq.map2R (+) (fseq [1 .. 4]) illFormedList |> Result.isError
    @>

[<Test>]
let ``pairing adjacent elements does not hang`` () = 
  let listWith5 = InfiniteSeq.init (MaxElements 10000) id |> InfiniteSeq.filter (fun i -> i < 5)
  test 
    <@
      InfiniteSeq.pairwise wellFormedList |> take 5 = Ok [(0,1); (1,2); (2,3); (3,4); (4,5)]
      &&
      InfiniteSeq.pairwise illFormedList |> take 1 |> Result.isError
      &&
      InfiniteSeq.pairwise listWith5 |> take 4 |> Result.isOk
      &&
      InfiniteSeq.pairwise listWith5 |> take 5 |> Result.isError
    @>

[<Test>]
let ``finding an element does not hang`` () = 
  test 
    <@
      InfiniteSeq.find' ((=) 8) wellFormedList = Ok 8
      &&
      InfiniteSeq.find' ((=) -2) wellFormedList |> Result.isError
      &&
      InfiniteSeq.find' ((=) 0) illFormedList |> Result.isError
    @>

[<Test>]
let ``zipping two infinite sequences does not hang`` () = 
  test
    <@
      InfiniteSeq.zip wellFormedList illFormedList |> take 1 |> Result.isError
      &&
      InfiniteSeq.zip illFormedList wellFormedList |> take 1 |> Result.isError
      &&
      InfiniteSeq.zip wellFormedList wellFormedList |> take 2 = Ok [(0, 0); (1, 1)]
    @>

[<Test>]
let ``zipping an infinite sequence with a finite sequence does not hang`` () = 
  test 
    <@
      InfiniteSeq.zipL wellFormedList (fseq [1 .. 4]) = Ok (fseq [ (0, 1); (1, 2); (2, 3); (3, 4) ])
      &&
      InfiniteSeq.zipL illFormedList (fseq [1 .. 4]) |> Result.isError
      &&
      InfiniteSeq.zipR (fseq [1 .. 4]) wellFormedList = Ok (fseq [ (1, 0); (2, 1); (3, 2); (4, 3) ])
      &&
      InfiniteSeq.zipR (fseq [1 .. 4]) illFormedList |> Result.isError
    @>


// [<Test>]
// let ``splits infinite sequences without hanging`` () = 
//   let alwaysFalse (_:int) = false
//   test 
//     <@
//       InfiniteSeq.split (fun i -> i % 20 = 0) (InfiniteSeq.init id) |> InfiniteSeq.take 3 |> ofNonEmpty
//         = [[0]; [1 .. 20]; [21 .. 40]]

//       &&

//       InfiniteSeq.splitPairwise (fun left right -> left % 20 = 0) (InfiniteSeq.init id) |> InfiniteSeq.take 3 |> ofNonEmpty
//         = [[0]; [1 .. 20]; [21 .. 40]]

//       &&

//       InfiniteSeq.split alwaysFalse (InfiniteSeq.init id) |> InfiniteSeq.head |> InfiniteSeq.take 40 |> FSeq.toList
//         = [0 .. 39]

//       &&

//       InfiniteSeq.splitPairwise (fun left right -> false) (InfiniteSeq.init id) |> InfiniteSeq.head |> InfiniteSeq.take 40 |> FSeq.toList 
//         = [0 .. 39]        
//     @>

