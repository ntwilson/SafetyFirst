namespace SafetyFirst

type Partial<'a> = Partial of (unit -> 'a)

module Partial = 
  let map f (Partial x) = 
    Partial (fun () -> f (x ()))

  let bind f (Partial x) : Partial<_> = 
    f (x ())
  
  let rtn x = Partial (fun () -> x)

  let run handler (Partial f) = 
    try f()
    with 
    | ex -> handler ex

  let unsafeRun (Partial f) = f ()

  let tryHandle handler (Partial f) =
    Partial (fun () -> 
      try f()
      with 
      | ex ->
        match handler ex with
        | Some x -> x
        | None -> reraise () 
    )

  let ofOption x = Partial (fun () -> Option.get x)
  let ofResult = function
    | Ok x -> rtn x
    | Error err -> Partial (fun () -> failwithf "%A" err)

type PartialCompExpr () = 
  member this.Bind (x, f) = Partial.bind f x
  member this.Return x = Partial.rtn x
  member this.ReturnFrom x = x
  member this.Delay f = 
    Partial (fun () ->
      let (Partial inner) = f ()
      inner ()
    ) 
  member this.TryFinally(body, compensation) =
    try 
      this.ReturnFrom(body())
    finally 
      compensation() 

  member this.Using(disposable:#System.IDisposable, body) =
    let body' = fun () -> body disposable
    this.TryFinally(body', fun () -> 
      match disposable with 
      | null -> () 
      | disp -> disp.Dispose())
            
  member this.Zero () = Partial.rtn ()

  member this.While(guard, body) =
    if not (guard()) 
    then 
      this.Zero() 
    else
      this.Bind( body(), fun () -> 
        this.While(guard, body))  

  member this.For(sequence:seq<_>, body) =
    this.Using(sequence.GetEnumerator(),fun enum -> 
      this.While(enum.MoveNext, fun () ->
        body enum.Current))


[<AutoOpen>]
module PartialExprBuilder =
  let partial = PartialCompExpr ()
