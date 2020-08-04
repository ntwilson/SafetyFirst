namespace SafetyFirst

open System.Runtime.CompilerServices

/// <summary>
/// Used for computation expressions.  Use <c>Result.expr</c> or <c>ResultExpression.result</c> 
/// to create the expression. 
/// </summary>
type ResultExpression () =
  member this.Bind (x, onOk) = Result.bind onOk x
  member this.Return x = Ok x
  member this.ReturnFrom x = x

[<AutoOpen>]
module ResultExpression =
  /// <summary>
  /// The computation expression to manage the Result monad.
  ///
  /// Code wrapped in a Result monad is known to potentially fail with specific errors.
  /// 
  /// Accessing <c>Result</c>'s value with <c>let!</c>, <c>do!</c>, <c>return!</c>, etc. methods in the computation expression
  /// will use the value of the Result if the Result is <c>Ok x</c>, or terminate early if the Result is <c>Error e</c>.
  /// </summary>
  let result = new ResultExpression ()

#nowarn "32121"

/// <summary>
/// Static functions for working with Result objects 
/// </summary>
module Result =
  open System

  /// <summary>
  /// Creates a new error context for the result.  This context can be repeatedly
  /// expanded upon through the <c>addContext</c> function.
  /// </summary>
  [<CompiledName("$notForC#_createContext")>] // since the compiled name isn't valid from C#, it effectively makes it visible only from F#
  let createContext errContext result = 
    result
    |> Result.mapError (fun e -> 
      { Error = e; Context = [errContext] })

  /// <summary>
  /// Expands on the error context created by the <c>createContext</c> function.
  /// </summary>
  [<CompiledName("$notForC#_addContext")>]
  let addContext newContext result = 
    result
    |> Result.mapError (fun ({ Context = context } as error) -> 
      { error with Context = newContext :: context })

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_bind2")>]
  let bind2 onOk result1 result2 =
    result {
      let! r1 = result1
      let! r2 = result2
      return! onOk r1 r2
    }

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_bind3")>]
  let bind3 onOk result1 result2 result3 =
    result {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return! onOk r1 r2 r3
    }

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_bind4")>]
  let bind4 onOk result1 result2 result3 result4 =
    result {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      let! r4 = result4
      return! onOk r1 r2 r3 r4
    }

  /// <summary>
  /// Collects a sequence of Results into a single Result of the sequence of values.
  /// If all of the Results are Ok, returns an Ok of the sequence of contained values.  
  /// If any of the Results are Error, returns an Error of the sequence of contained 
  /// Errors (and throws away any Ok values). 
  /// <c>collect [Ok 1; Ok 2; Ok 3]</c> would return <c>Ok [1; 2; 3]</c>, but 
  /// <c>collect [Ok 1; Error "err"; Error "fail"]</c> would return <c>Error ["err"; "fail"]</c>
  /// </summary>
  [<CompiledName("$notForC#_collect")>]
  let collect results =
    let rec collect' state xs =
      match (state, xs) with
      | (Ok xs, Ok x :: tail) -> collect' (Ok (x :: xs)) tail
      | (Ok _, Error e :: tail) -> collect' (Error [e]) tail
      | (Error errs, Error e :: tail) -> collect' (Error (e :: errs)) tail
      | (Error _ as errors, Ok _ :: tail) -> collect' errors tail
      | (Ok xs, []) -> Ok <| List.rev xs
      | (Error errs, []) -> Error <| List.rev errs

    in collect' (Ok []) (Seq.toList results)

  let private concatResults results =
    let rec concat state rs =
      match rs with
      | head::tail -> 
        match head with 
        | Ok x -> concat (x::state) tail
        | Error err -> Error err
      | [] -> Ok (state |> Seq.ofList |> Seq.rev)

    concat [] (results |> Seq.toList)

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_bindAll")>]
  let bindAll onOk results = 
    concatResults results
    |> Result.bind onOk

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_map2")>]
  let map2 onOk result1 result2 = 
    result {
      let! r1 = result1
      let! r2 = result2
      return onOk r1 r2
    }

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_map3")>]
  let map3 onOk result1 result2 result3 = 
    result {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      return onOk r1 r2 r3
    }

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_map4")>]
  let map4 onOk result1 result2 result3 result4 =
    result {
      let! r1 = result1
      let! r2 = result2
      let! r3 = result3
      let! r4 = result4
      return onOk r1 r2 r3 r4
    }
          
  /// <summary>      
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompiledName("$notForC#_mapAll")>]
  let mapAll onOk results = 
    concatResults results
    |> Result.map onOk

  /// <summary>
  /// Returns true when given a Result that is Ok, and false when given a Result that is Error.
  /// </summary>
  [<CompiledName("$notForC#_isOk")>]
  let isOk = function | Ok _ -> true | Error _ -> false

  /// <summary>
  /// Returns true when given a Result that is Error, and false when given a Result that is Ok.
  /// </summary>
  [<CompiledName("$notForC#_isError")>]
  let isError = function | Error _ -> true | Ok _ -> false 

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it 
  /// to the function passed in.  Does nothing if the Result is an error
  /// </summary>
  [<CompiledName("$notForC#_ifOk")>]
  let ifOk ok result = 
    match result with
    | Ok v -> ok v
    | Error _ -> () 

  /// <summary>
  /// If the Result is error, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is an ok.
  /// </summary>
  [<CompiledName("$notForC#_ifError")>]
  let ifError error result = 
    match result with
    | Ok _ -> ()
    | Error err -> error err

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, returns the default value given.
  /// </summary>
  [<CompiledName("$notForC#_defaultValue")>]
  let defaultValue value result = 
    match result with
    | Ok v -> v
    | Error err -> value

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, constructs the default value from the function given
  /// and returns it.
  /// </summary>
  [<CompiledName("$notForC#_defaultWith")>]
  let defaultWith valueF result =
    match result with
    | Ok v -> v
    | Error err -> valueF err

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// specified message along with the error details
  /// </summary>
  [<CompiledName("$notForC#_unless")>]
  let unless msg result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException (msg, err))

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException with a message 
  /// produced by the input function along with the error details
  /// </summary>
  [<CompiledName("$notForC#_unlessErr")>]
  let unlessErr msg result =
    match result with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException (msg err, err))

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// error details
  /// </summary>
  [<CompiledName("$notForC#_expect")>]
  let expect result = 
    match result with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException err)

  /// <summary>
  /// Converts a SafetyFirst.CSharp.Result type to an FSharpResult type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  [<CompiledName("$notForC#_ofCs")>]
  let ofCs (result:SafetyFirst.CSharp.Result<_,_>) = 
    result.ToFs()

  /// <summary>
  /// Converts an FSharpResult type to a ResultDotNet Result type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  [<CompiledName("$notForC#_toCs")>]
  let toCs result = 
    match result with
    | Ok ok -> SafetyFirst.CSharp.Result.Ok ok
    | Error err -> SafetyFirst.CSharp.Result.Error err

  /// <summary>
  /// Converts an <c>Option</c> of <c>'a</c> to a <c>Result</c> of <c>'a</c> and <c>'err</c>
  /// given some <c>'err</c> to use if the <c>Option</c> is <c>None</c> 
  /// </summary>
  [<CompiledName("$notForC#_ofOption")>]
  let ofOption err opt = 
    match opt with
    | Some v -> Ok v
    | None -> Error err

  /// <summary>
  /// Converts an <c>Option</c> of <c>'a</c> to a <c>Result</c> of <c>'a</c> and <c>'err</c>
  /// given a delayed function of <c>'err</c> to use if the <c>Option</c> is <c>None</c> 
  /// </summary>
  [<CompiledName("$notForC#_ofOptionWith")>]
  let ofOptionWith errThunk opt = 
    match opt with
    | Some v -> Ok v
    | None -> Error (errThunk ())

  /// <summary>
  /// Converts a <c>Result</c> to an <c>Option</c>, throwing away the <c>Error</c> information
  /// if the <c>Result</c> was an <c>Error</c>
  /// </summary>
  [<CompiledName("$notForC#_toOption")>]
  let toOption result = 
    match result with
    | Ok v -> Some v
    | Error _ -> None  





  open SafetyFirst.CSharp

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is error, returns the error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Bind onOk (result:Result<_,_>) = result.Bind onOk

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Bind2 (onOk:Func<'a, 'b, Result<'c, 'd>>) (result1:Result<'a,'d>) (result2:Result<'b,'d>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> onOk.Invoke (r1, r2)))

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Bind3 (onOk:Func<'a, 'b, 'c, Result<'d, 'e>>) (result1:Result<'a,'e>) (result2:Result<'b,'e>) (result3:Result<'c,'e>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> 
        result3.Bind(fun r3 -> onOk.Invoke (r1, r2, r3))))

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Bind4 (onOk:Func<'a, 'b, 'c, 'd, Result<'e, 'f>>) (result1:Result<'a,'f>) (result2:Result<'b,'f>) (result3:Result<'c,'f>) (result4:Result<'d,'f>) = 
    result1.Bind(fun r1 ->
      result2.Bind(fun r2 -> 
        result3.Bind(fun r3 -> 
          result4.Bind(fun r4 -> onOk.Invoke (r1, r2, r3, r4)))))

  let private concatResultsCs results =
    let rec concat state rs =
      match rs with
      | head::tail -> 
        match head with 
        | Ok x -> concat (x::state) tail
        | Error err -> Error err
      | [] -> Ok (state |> Seq.ofList |> Seq.rev)

    concat [] (results |> Seq.toList)

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes them
  /// to the function given, returning the result of that function.  If any Result 
  /// is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let BindAll onOk results = 
    (concatResultsCs results).Bind onOk

  /// <summary>
  /// If all the Results are ok, "unwraps" the ok values and passes it
  /// to the function given, returning an Ok with the result of that function.  
  /// If the Result is error, returns the error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Map onOk (result:Result<_,_>) = result.Map onOk

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Map2 (onOk:Func<'a, 'b, 'c>) result1 result2 = 
    Bind2 (Func<'a, 'b, Result<'c, 'd>> (fun r1 r2 -> Ok (onOk.Invoke (r1, r2)))) result1 result2

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Map3 (onOk:Func<'a, 'b, 'c, 'd>) result1 result2 result3 = 
    Bind3 (Func<'a, 'b, 'c, Result<'d, 'e>> (fun r1 r2 r3 -> Ok (onOk.Invoke (r1, r2, r3)))) 
      result1 result2 result3

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Map4 (onOk:Func<'a, 'b, 'c, 'd, 'e>) result1 result2 result3 result4 = 
    Bind4 (Func<'a, 'b, 'c, 'd, Result<'e, 'f>> (fun r1 r2 r3 r4 -> Ok (onOk.Invoke (r1, r2, r3, r4)))) 
      result1 result2 result3 result4

  /// <summary>
  /// If the Result is ok, "unwraps" the ok values and passes them
  /// to the function given, returning an Ok with the result of that function.  
  /// If any Result is error, returns the first error without calling the function given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let MapAll onOk results = 
    (concatResultsCs results).Map onOk

  /// <summary>
  /// Creates a new ok Result with the value given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Ok<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Ok v

  /// <summary>
  /// Creates a new error Result with the error object given. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Error<'tVal, 'tErr> v : Result<'tVal, 'tErr> = Error v

  /// <summary>
  /// Converts an FSharpResult type to a ResultDotNet Result type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let FromFs v = toCs v

  /// <summary>
  /// Collects a sequence of Results into a single Result of the sequence of values.
  /// If all of the Results are Ok, returns an Ok of the sequence of contained values.  
  /// If any of the Results are Error, returns an Error of the sequence of contained 
  /// Errors (and throws away any Ok values). 
  /// <c>Result.Collect [Ok(1), Ok(2), Ok(3)]</c> would return <c>Ok([1, 2, 3])</c>, but 
  /// <c>Result.Collect [Ok(1), Error("err"), Error("fail")]</c> would return <c>Error(["err", "fail"])</c>
  /// </summary>
  [<CompilerMessage(message="not for use from F#", messageNumber=32121, IsHidden=true)>]
  let Collect (results:seq<Result<'a,'b>>) = 
    results 
    |> Seq.map (fun r -> r.ToFs ())
    |> collect
    |> Result.map fseq
    |> Result.mapError fseq
    |> FromFs

[<AutoOpen>]
[<Extension>]
module FsResultExtensions = 
  /// <summary>
  /// Converts to a ResultDotNet Result type.  Used for interoping between C# and F# codebases.
  /// </summary>
  [<Extension>]
  let ToCs v = Result.FromFs v
