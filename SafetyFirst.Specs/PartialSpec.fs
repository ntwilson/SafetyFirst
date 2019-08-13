module SafetyFirst.Specs.PartialSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open System

let always x _ = x

[<Test>]
let ``can run a partial computation expression`` () =
  let workingPartial = 
    partial {
      return 5
    }
  
  let failingPartial = 
    partial {
      return failwith "FAYLZZZ"
    }

  test 
    <@
      Partial.run (always 2) workingPartial = 5
      &&
      Partial.run (always 2) failingPartial = 2 
    @>  


[<Test>]
let ``can make complicated partial computation expressions that include for loops`` () = 
  let mutable isDisposed = false 
  let someDisposable () = { new IDisposable with member this.Dispose () = isDisposed <- true }

  let mutable i = 0
  let complexPartial = 
    partial { 
      use x = someDisposable ()
      for j in 1 .. 5 do
        i <- i + 1
    }

  Partial.run (always ()) complexPartial

  test <@ i = 5 && isDisposed @>

[<Test>]
let ``can map partials`` () = 
  let myPart = partial { return 5 }
  let ans = myPart |> Partial.map ((+) 2)
  test <@ Partial.unsafeRun ans = 7 @>

[<Test>]
let ``can try handling an error but still return a partial `` () = 
  let firstRun = 
    partial { return failwith "FAYLZZZ" }
    |> Partial.tryHandle 
      (function 
        | :? InvalidOperationException as x -> Some 2
        | _ -> None)

  let secondRun = 
    firstRun 
    |> Partial.tryHandle 
      (always (Some 5))


  test 
    <@ 
      Partial.run (always 10) firstRun = 10
      &&
      Partial.run (always 10) secondRun = 5 
    @>
