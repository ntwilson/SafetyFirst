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

  /// <summary>
  /// The computation expression to manage the Option monad.
  ///
  /// Code wrapped in a Option monad is known to potentially fail.
  /// 
  /// Accessing Option<'a> values with let!, do!, return!, etc. methods in the computation expression
  /// will use the value of the result if Some x, or terminate early if the Option is None.
  /// </summary>
  let option = OptionExpression ()      