namespace SafetyFirst

open System

[<AutoOpen>]
module private ResultExceptionMessageHelper = 
  let inline formatExceptionMessage msg (err:obj) = 
    sprintf "%s%sResult error details: %s" msg Environment.NewLine (string err)  

type ResultExpectedException<'T> (msg:string, err:'T) =
  inherit Exception (formatExceptionMessage msg err)

  new (err:'T) =
    ResultExpectedException 
      (sprintf "Result returned with error.  %s" (formatExceptionMessage "" err), 
       err)
  member this.ErrorDetails = err

type ErrorWithContext<'a> = { Error : 'a; Context : string list }

namespace SafetyFirst.CSharp

open System
open SafetyFirst

/// <summary>
/// Represents the outcome of a calculation that could have failed.
/// For example, a divide function might return a Result, with an Error
/// if the denominator was 0, and an Ok in any other case.
/// <c>Result</c> is a union of <c>Ok</c> of <c>tVal</c> and <c>Error</c> of <c>tErr</c>.   
/// <c>tVal</c> represents the type of the expected result, and <c>tErr</c> represents
/// the type used to represent the error (such as a <c>string</c> if just using
/// error messages)
/// </summary>
type Result<'tVal, 'tErr> = 
  | Ok of 'tVal
  | Error of 'tErr

   
  /// <summary>
  /// "Unwraps" the ok value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Funcs: one to execute if the result is ok, and one
  /// to execute if the result is an error.  Returns the result of executing
  /// the appropriate function.
  /// </summary>
  member this.Match (ok:Func<'tVal, 'a>, error:Func<'tErr, 'a>) =
    match this with
    | Ok v -> ok.Invoke v
    | Error err -> error.Invoke err

  /// <summary>
  /// "Unwraps" the ok value or the error object and passes it 
  /// to the function passed in.
  /// Takes two Actions: one to execute if the result is ok, and one 
  /// to execute if the result is an error.
  /// </summary>
  member this.Match (ok:Action<'tVal>, error:Action<'tErr>) =
    this.Match (
      Func<'tVal, unit> ok.Invoke,
      Func<'tErr, unit> error.Invoke)

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it 
  /// to the function passed in.  Does nothing if the Result is an error
  /// </summary>
  member this.IfOk (ok:Action<'tVal>) =
    this.Match (ok, ignore)

  /// <summary>
  /// If the Result is error, "unwraps" the error object and passes it 
  /// to the function passed in.  Does nothing if the Result is an ok
  /// </summary>
  member this.IfError (error:Action<'tErr>) =
    this.Match (ignore, error)

  /// <summary>
  /// If the result is error, returns the defaultValue passed in.  If 
  /// the result is ok, "unwraps" the ok value and returns it.
  /// </summary>
  member this.OkOrElse defaultValue =
    match this with
    | Ok v -> v
    | Error _ -> defaultValue

  /// <summary>
  /// If the result is error, returns the result of the defaultValueFunc 
  /// passed in.  If the result is ok, "unwraps" the ok value
  /// and returns it.
  /// </summary>
  member this.OkOrElse (defaultValueFunc:Func<'tVal>) =
    match this with
    | Ok v -> v
    | Error _ -> defaultValueFunc.Invoke ()

  /// <summary>
  /// If the result is error, returns the result of the defaultValueFunc 
  /// passed in.  If the result is ok, "unwraps" the ok value
  /// and returns it.
  /// </summary>
  member this.OkOrElse (defaultValueFunc:Func<'tErr, 'tVal>) =
    match this with
    | Ok v -> v
    | Error err -> defaultValueFunc.Invoke err

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.Bind (onOk:Func<'tVal, Result<'a, 'tErr>>) =
    match this with
    | Ok v -> onOk.Invoke v
    | Error err -> Error err

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning an Ok with the result of that function.  
  /// If the Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.Map (onOk:Func<'tVal, 'a>) = 
    match this with
    | Ok v -> Ok (onOk.Invoke v)
    | Error err -> Error err

  /// <summary>
  /// If the Result is ok, returns the ok value without calling the function given.
  /// If the Result is error, "unwraps" the error value and passes it
  /// to the function given, returning an Error with the result of that function.  
  /// </summary>
  member this.MapError (onErr:Func<'tErr, 'a>) = 
    match this with
    | Ok v -> Ok v
    | Error err -> Error (onErr.Invoke err)


  /// <summary>
  /// Converts a ResultDotNet Result type to an FSharpResult type.  Used for interoping between
  /// C# and F# codebases. 
  /// </summary>
  member this.ToFs () = 
    match this with
    | Ok v -> FSharp.Core.Ok v
    | Error err -> FSharp.Core.Error err

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning the result of that function.  If the 
  /// Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.SelectMany<'uVal, 'vVal> (func : Func<'tVal, Result<'uVal, 'tErr>>) (projection : Func<'tVal, 'uVal, 'vVal>) =
    this.Bind (fun t -> (func.Invoke t).Map (fun u -> projection.Invoke (t, u)))

  /// <summary>
  /// If the Result is ok, "unwraps" the ok value and passes it
  /// to the function given, returning an Ok with the result of that function.  
  /// If the Result is error, returns the error without calling the function given. 
  /// </summary>
  member this.Select<'u>(func : Func<'tVal, 'u>) =
    this.Map func

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// specified message along with the error details
  /// </summary>
  member this.Unless (msg) =
    match this with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException (msg, err))   

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException with a message 
  /// produced by the input function along with the error details
  /// </summary>
  member this.Unless (msg:Func<'tErr, string>) = 
    match this with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException (msg.Invoke err, err))

  /// <summary>
  /// If the Result is Ok, "unwraps" the result and returns it.
  /// If the Result is Error, throws a ResultExpectedException containing the 
  /// the error details
  /// </summary>
  member this.Expect () = 
    match this with
    | Ok v -> v
    | Error err -> raise (ResultExpectedException err)

open System.Runtime.CompilerServices
[<Extension>]
type ResultExtensions private () = 

  /// <summary>
  /// Creates a new error context for the result.  This context can be repeatedly
  /// expanded upon through repeat calls to the <c>WithContext</c> method.
  /// </summary>
  [<Extension>]
  static member WithContext (result:Result<'a, ErrorWithContext<'b>>, contextStr) = 
    result.MapError(fun ({Context = context} as error) -> { error with Context = contextStr :: context })

  /// <summary>
  /// Expands on the error context created earlier by the <c>WithContext</c> function.
  /// </summary>
  [<Extension>]
  static member WithContext (result:Result<'a, 'b>, contextStr) = 
    result.MapError(fun e -> { Error = e; Context = [contextStr] })
