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
  /// Accessing </c>Option</c> values with <c>let!</c>, <c>do!</c>, <c>return!</c>, etc. methods in the computation expression
  /// will use the value of the Option if the Option is <c>Some x</c>, or terminate early if the Option is <c>None</c>.
  /// </summary>
  let option = OptionExpression ()      