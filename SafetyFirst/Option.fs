module SafetyFirst.Option

open System

type OptionExpectedException (msg) = 
  inherit Exception (msg)

let unless msg maybe = 
  match maybe with
  | Some x -> x
  | None -> raise (OptionExpectedException msg)

let collect maybes = 
  let rec collect' state xs =
    match xs with
    | None :: _ -> None
    | Some element :: tail -> collect' (element :: state) tail
    | [] -> Some <| List.rev state

  in collect' [] (Seq.toList maybes)

/// <summary>
/// Maps a sequence by some projection and sequences the resulting Options into a single Option 
/// of the sequence of values.
/// If all of the Options are Some, returns a Some of the sequence of projected values.  
/// If any of the Options are None, returns None and does not evaluate any of the rest of the sequence.
/// <c>traverse (flip Array.tryItem [|1..4|]) [1; 2; 3]</c> would return <c>Some [2; 3; 4]</c>, but 
/// <c>traverse (flip Array.tryItem [|1..4|]) [1; 5; 2]</c> would return <c>None</c> after 
/// trying to grab the 5th element.
/// </summary>
let traverse fn (maybes : _ seq) = 
  let enum = maybes.GetEnumerator ()
  let mutable hitNone = false
  let okVals = 
    [
      while hitNone = false && enum.MoveNext () do
        match fn enum.Current with
        | None -> hitNone <- true
        | Some o -> yield o
    ]
  
  if hitNone 
  then None
  else Some okVals

/// <summary>
/// Collects a sequence of Options into a single Option of the sequence of values.
/// If all of the Options are Some, returns an Some of the sequence of contained values.  
/// If any of the Options are None, returns None and does not evaluate any of the rest of the sequence.
/// <c>sequence [Some 1; Some 2; Some 3]</c> would return <c>Some [1; 2; 3]</c>, but 
/// <c>sequence [Some 1; None; None]</c> would return <c>None</c>
/// </summary>
let sequence maybes = traverse id maybes

let ofFloat (x:float) = 
  if Double.IsNaN x 
  then None
  else Some x 

let ofResult = function
  | Ok v -> Some v
  | Error _ -> None

let toResult err = function
  | Some v -> Ok v
  | None -> Error err

let toResultWith errThunk = function
  | Some v -> Ok v
  | None -> Error (errThunk ())

let ofVOption = function
  | ValueSome v -> Some v
  | ValueNone -> None

let toVOption = function
  | Some v -> ValueSome v
  | None -> ValueNone
  