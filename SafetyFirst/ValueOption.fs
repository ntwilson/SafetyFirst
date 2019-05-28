module SafetyFirst.ValueOption

open System

let unless msg maybe = 
  match maybe with
  | ValueSome x -> x
  | ValueNone -> raise (Option.OptionExpectedException msg)

let collect maybes = 
  Seq.fold 
    (fun state element -> 
      match state, element with
      | (ValueSome xs, ValueSome x) -> ValueSome (Seq.append xs [x])
      | _ -> ValueNone)
    (ValueSome (upcast []))
    maybes

let ofFloat (x:float) = 
  if Double.IsNaN x 
  then ValueNone
  else ValueSome x 

let ofResult = function
  | Ok v -> ValueSome v
  | Error _ -> ValueNone

let toResult err = function
  | ValueSome v -> Ok v
  | ValueNone -> Error err

let toResultWith errThunk = function
  | ValueSome v -> Ok v
  | ValueNone -> Error (errThunk ())

let ofOption = function
  | Some v -> ValueSome v
  | None -> ValueNone

let toOption = function 
  | ValueSome v -> Some v
  | ValueNone -> None

  