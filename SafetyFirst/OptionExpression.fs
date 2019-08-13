namespace SafetyFirst

type OptionExpression () =
  member this.Bind (maybeX, f) = 
    match maybeX with
    | Some x -> f x
    | None -> None

  member this.Return x = 
    Some x

  member this.ReturnFrom x = 
    x


[<AutoOpen>]
module OptionExpression =
  let option = OptionExpression ()      