module SafetyFirst.ValueOption

open System

let unless msg maybe = 
  match maybe with
  | ValueSome x -> x
  | ValueNone -> raise (Option.OptionExpectedException msg)

let collect maybes = 
  let rec collect' state xs =
    match xs with
    | ValueNone :: _ -> ValueNone
    | ValueSome element :: tail -> collect' (element :: state) tail
    | [] -> ValueSome <| List.rev state

  in collect' [] (Seq.toList maybes)

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

  