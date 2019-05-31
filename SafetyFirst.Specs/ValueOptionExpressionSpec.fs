module SafetyFirst.Specs.ValueOptionExpressionSpec 

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``Returns ValueSome value for multiple voptions`` () =
  let result = 
    voption {
      let! x = ValueSome 5
      let! y = ValueSome 10
      return x + y      
    }

  test <@ result = ValueSome 15 @>

let ``Returns ValueNone if any value in a result expression is ValueNone`` () =
  test 
    <@
      voption {
        let! x = ValueNone
        let! y = ValueSome 10
        return x + y
      } = ValueNone

      &&

      voption {
        let! x = ValueSome 5
        let! y = ValueNone
        return x + y        
      } = ValueNone

      &&
    
      voption { 
        let! x = ValueSome 5
        let! y = ValueSome 10
        return! ValueNone        
      } = ValueNone
  @>

[<Test>]
let ``Returns early if it encounters a ValueNone`` () =  
  let mutable didExecute = false

  let result = 
    voption { 
      let! x = ValueNone
      didExecute <- true
      let! y = ValueSome 10
      return x + y
    }

  test <@ result = ValueNone && not didExecute @>

[<Test>]
let ``Returns from a given voption`` () = 
  test
    <@
      voption {
        let! x = ValueSome 5
        let! y = ValueSome 10
        return! ValueSome (x + y)
      } = ValueSome 15

      &&

      voption { 
        let! x = ValueSome 5
        let! y = ValueSome 10
        return! ValueNone
      } = ValueNone
    @>
