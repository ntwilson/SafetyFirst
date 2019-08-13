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
  