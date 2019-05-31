module SafetyFirst.Specs.IEnumerableSpec 

open System
open System.Linq
open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.CSharp

let throws f = 
  try 
    Lazy.force f |> ignore
    false
  with 
  | _ -> true

let isError = function | Error _ -> true | Ok _ -> false

let errorsAtTheSameTime safeVersion unsafeVersion = 
  isError safeVersion = throws unsafeVersion

let sameAnswer safeVersion unsafeVersion = 
  match safeVersion with
  | Ok safeAns -> 
    let unsafeAns = Lazy.force unsafeVersion
    safeAns = unsafeAns
  | Error _ -> true  

let sameBehaviorFor behavior (seqFactory:int -> int -> _ seq) = 
  let mySeq = seqFactory 0 5
  let myEmptySeq = seqFactory 0 -1
  in 
    behavior (mySeq.ElementAtSafe 0) (lazy (mySeq.ElementAt 0))
    &&
    behavior (mySeq.ElementAtSafe -1) (lazy (mySeq.ElementAt -1))
    &&
    behavior (mySeq.ElementAtSafe 5) (lazy (mySeq.ElementAt 5))
    &&
    behavior (mySeq.ElementAtSafe 6) (lazy (mySeq.ElementAt 6))
    && 
    behavior (myEmptySeq.ElementAtSafe 0) (lazy (myEmptySeq.ElementAt 0))
    &&
    behavior (myEmptySeq.ElementAtSafe -1) (lazy (myEmptySeq.ElementAt -1))
    &&
    behavior (myEmptySeq.ElementAtSafe 1) (lazy (myEmptySeq.ElementAt 1))


[<Test>]
let ``ElementAtSafe errors whenever ElementAt throws`` () = 
  let testFor = sameBehaviorFor errorsAtTheSameTime
  test 
    <@
      testFor (fun lower upper -> seq { lower .. upper })
      &&
      testFor (fun lower upper -> upcast [ lower .. upper ]) 
      &&
      testFor (fun lower upper -> upcast [| lower .. upper |])
      &&
      testFor (fun lower upper -> upcast ResizeArray { lower .. upper })
      &&
      testFor (fun lower upper -> upcast System.Collections.Generic.LinkedList { lower .. upper })
    @>    

[<Test>]
let ``ElementAtSafe returns the same answer as ElementAt`` () = 
  let testFor = sameBehaviorFor sameAnswer
  test
    <@ 
      testFor (fun lower upper -> seq { lower .. upper })
      &&
      testFor (fun lower upper -> upcast [ lower .. upper ]) 
      &&
      testFor (fun lower upper -> upcast [| lower .. upper |])
      &&
      testFor (fun lower upper -> upcast ResizeArray { lower .. upper })
      &&
      testFor (fun lower upper -> upcast System.Collections.Generic.LinkedList { lower .. upper })
    @>

[<Test>]
let ``ZipSafe errors whenever lengths mismatch`` () = 
  let add : Func<_,_,_> = Func<_,_,_> (fun a b -> a + b)

  let expectedErrors = 
    [ 
      (seq { 0 .. 4 }).ZipSafe(seq { 0 .. 5 }, add) 
      Seq.empty.ZipSafe(seq { 0 .. 5 }, add)
      (seq { 0 .. 4 }).ZipSafe(Seq.empty, add)
    ]

  test <@ Seq.forall isError expectedErrors @>

let ansFromZip (a:_ seq) (b:_ seq) (f:'a -> 'b -> 'c) = a.Zip(b, Func<_,_,_> f) |> Seq.toList
let ansFromZipSafe (a:_ seq) (b:_ seq) (f: 'a -> 'b -> 'c) = a.ZipSafe(b, Func<_,_,_> f).Expect() |> Seq.toList
let sameAns (a, b, f) = 
  (ansFromZip a b f) = (ansFromZipSafe a b f)

[<Test>]
let ``ZipSafe returns the same answer as Zip`` () = 
  test
    <@
      sameAns (seq { 0 .. 4}, seq { 0 .. 4 }, (+))
      &&
      sameAns (Seq.empty<int>, Seq.empty<int>, (+))
      &&
      sameAns (seq [1], seq ["hi"], (fun i s -> s + string i))
    @>
