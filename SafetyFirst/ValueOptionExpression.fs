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

  member this.Zero () = ValueSome ()
  member this.Combine (maybeUnit, rest) = this.Bind (maybeUnit, rest)

  member this.Delay f = f
  member this.Run f = f ()


[<AutoOpen>]
module ValueOptionExpression =
  let voption = ValueOptionExpression ()
