module SafetyFirst.Specs.InfiniteSeqSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Numbers

open InfiniteSeq

let always x _ = x
let wellFormedList = init (MaxElements 10000) id
let illFormedList = init (MaxElements 1000) (always 0) |> filter ((<>) 0)
let hung = InfiniteSeqHung "Program execution hung.  This infinite sequence was allowed to evaluate elements for too long."

let take n xs = take' n xs |> Result.map Seq.toList


[<Test>]
let ``grabbing an item at a given index does not hang`` () = 
  test
    <@
      wellFormedList |> item' (NaturalInt.assume 100) = Ok 100
      &&
      illFormedList |> item' (NaturalInt.assume 100) |> Result.isError
    @>

[<Test>]
let ``eagerly taking elements does not hang`` () = 
  test 
    <@
      wellFormedList |> take' 5 |> Result.map Seq.toList = Ok [0..4]
      &&
      illFormedList |> take' 5 |> Result.isError
    @>

[<Test>]
let ``lazily taking elements does not hang`` () = 
  let listWith5 = init (MaxElements 1000) (fun i -> i - 5) |> filter (fun i -> i < 0)

  test 
    <@
      listWith5 |> truncate 4 |> Seq.toList = ([-5 .. -2] |> List.map Ok)
      &&
      listWith5 |> truncate 7 |> Seq.toList = ([-5 .. -1] |> List.map Ok) @ [Error hung]
    @>

[<Test>]
let ``eagerly taking while some predicate does not hang`` () = 
  test 
    <@
      wellFormedList |> takeWhile' (fun i -> i < 10) |> Result.map Seq.toList = Ok [0 .. 9]
      &&
      illFormedList |> takeWhile' (fun i -> i < 10) |> Result.isError
    @>

[<Test>]
let ``lazily taking while some predicate can return an infinite result and does not hang`` () =
  test 
    <@
      wellFormedList |> takeWhileLazy (always true) |> Seq.take 10 |> Seq.toList = ([0 .. 9] |> List.map Ok)
      &&
      illFormedList |> takeWhileLazy (always true) |> Seq.toList = [Error hung]
      &&
      wellFormedList |> takeWhileLazy (fun i -> i < 10) |> Seq.toList = ([0 .. 9] |> List.map Ok)
    @>    

[<Test>]
let ``choosing by a function returning an option does not hang`` () = 
  let even i =  
    if i % 2 = 0 
    then Some i
    else None

  test 
    <@
      wellFormedList |> choose even |> take 3 = Ok [0;2;4]
      &&
      illFormedList |> choose even |> take 3 |> Result.isError
    @>

[<Test>]
let ``chunking by some finite size does not hang`` () = 
  let listWith5 = init (MaxElements 1000) (fun i -> i - 4) |> filter (fun i -> i <= 0)
  test
    <@
      wellFormedList |> chunkBySizeUnsafe 2 |> take 3
        = Ok [ [|0;1|]; [|2;3|]; [|4;5|] ]
      &&
      illFormedList |> chunkBySizeUnsafe 2 |> take 3 |> Result.isError
      &&
      listWith5 |> chunkBySizeUnsafe 2 |> take 2 = Ok [ [|-4;-3|]; [|-2;-1|] ]
      &&
      listWith5 |> chunkBySizeUnsafe 2 |> take 3 |> Result.isError
    @>

[<Test>]
let ``skipping does not hang`` () = 
  test 
    <@
      wellFormedList |> skip 5 |> take 5 = Ok [5 .. 9]
      &&
      illFormedList |> skip 5 |> take 5 |> Result.isError 
      &&
      wellFormedList |> skipWhile (fun i -> i < 10) |> take 5 = Ok [10 .. 14]
      &&
      illFormedList |> skipWhile (fun i -> i < 10) |> take 5 |> Result.isError
      &&
      wellFormedList |> skipWhile (always true) |> take 1 |> Result.isError
    @>


