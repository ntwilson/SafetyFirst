module SafetyFirst.Specs.IEnumerableSpec 

open System.Linq
open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

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
