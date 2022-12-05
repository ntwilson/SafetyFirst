module SafetyFirst.Specs.OptionExpressionSpec 

open System
open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``Returns Some value for multiple options`` () =
  let result = 
    option {
      let! x = Some 5
      let! y = Some 10
      return x + y
    }

  test <@ result = Some 15 @>

let ``Returns None if any value in a result expression is None`` () =
  test 
    <@
      option {
        let! x = None
        let! y = Some 10
        return x + y
      } = None

      &&

      option {
        let! x = Some 5
        let! y = None
        return x + y        
      } = None

      &&
    
      option { 
        let! x = Some 5
        let! y = Some 10
        return! None        
      } = None
  @>

[<Test>]
let ``Returns early if it encounters a None`` () =  
  let mutable didExecute = false

  let result = 
    option { 
      let! x = None
      didExecute <- true
      let! y = Some 10
      return x + y
    }

  test <@ result = None && not didExecute @>

[<Test>]
let ``Returns from a given option`` () = 
  test
    <@
      option {
        let! x = Some 5
        let! y = Some 10
        return! Some (x + y)
      } = Some 15

      &&

      option { 
        let! x = Some 5
        let! y = Some 10
        return! None
      } = None
    @>

[<Test>]
let ``disposes anything that is used`` () =
  let mutable bareUseDisposed = false
  let mutable wrappedUseDisposed = false
  let result = option { 
    use a = { new IDisposable with member this.Dispose () = bareUseDisposed <- true }
    use! b = Some { new IDisposable with member this.Dispose () = wrappedUseDisposed <- true }
    let! c = None
    return 5
  }

  test <@ result = None && bareUseDisposed && wrappedUseDisposed @>

[<Test>]
let ``catches exceptions`` () = 
  let mutable exceptionCaught = false
  let mutable cleanedUp = false

  let result = 
    try
      option {
        try
          try 
            failwith "this threw"
            return 5
          with
          | e -> 
            exceptionCaught <- true
            return raise e
        finally
          cleanedUp <- true
      }
    with 
    | e -> None

  test <@ result = None && exceptionCaught && cleanedUp @>
