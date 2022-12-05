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

  member this.Delay f = f
  member this.Run f = f ()

  member this.TryWith(body, handler) =
    try this.ReturnFrom(body()) with
    | e -> handler e

  member this.TryFinally(body, compensation) =
    try this.ReturnFrom(body())
    finally
      compensation()

  member this.Using(disposable:#System.IDisposable, body) =
    let body' = fun () -> body disposable
    this.TryFinally(body', fun () ->
      match disposable with
      | null -> ()
      | disp -> disp.Dispose()
    )


[<AutoOpen>]
module OptionExpression =

  /// <summary>
  /// The computation expression to manage the Option monad.
  ///
  /// Code wrapped in a Option monad is known to potentially fail.
  /// 
  /// Accessing <c>Option</c> values with <c>let!</c>, <c>do!</c>, <c>return!</c>, etc. methods in the computation expression
  /// will use the value of the Option if the Option is <c>Some x</c>, or terminate early if the Option is <c>None</c>.
  /// </summary>
  let option = OptionExpression ()      