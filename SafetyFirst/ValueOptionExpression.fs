namespace SafetyFirst

type ValueOptionExpression () =
  member this.Bind (maybeX, f) = 
    match maybeX with
    | ValueSome x -> f x
    | ValueNone -> ValueNone

  member this.Return x = 
    ValueSome x

  member this.ReturnFrom x = 
    x


[<AutoOpen>]
module ValueOptionExpression =
  let voption = ValueOptionExpression ()      