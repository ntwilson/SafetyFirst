module SafetyFirst.Specs.OptionSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

let x = ()

[<Test>]
let ``Converts NaNs to Nones, but any other float to Some`` () =
  test
    <@
      Option.ofFloat nan = None
      &&
      Option.ofFloat 5. = Some 5.
    @>

let flip f a b = f b a

[<Test>]
let ``Collects multiple Options such that any None will make the whole thing None`` () =
  test
    <@
      Option.collect [ Some 1; Some 2; Some 3; Some 4 ] = Some [1 .. 4]
      &&
      Option.collect [ None; Some 2; Some 3; Some 4 ] = None
      &&
      Option.collect [ Some 1; None; Some 3; Some 4 ] = None
      &&
      Option.collect [ Some 1; Some 2; Some 3; None ] = None      
    @>

  test 
    <@ 
      Option.traverse (flip Array.tryItem [|1..4|]) [1;2;3] = Some [2;3;4]
      &&
      Option.traverse (flip Array.tryItem [|1..4|]) [1;5;2] = None
    @>

  test 
    <@
      Option.sequence [Some 1; Some 2; Some 3] = (Some [1; 2; 3])
      &&
      Option.sequence [Some 1; None; None] = None
      &&
      Option.sequence [] = Some []
    @>
  
let expectException act = 
  try
    let (Lazy value) = act 
    raise (AssertionException "Expecting an Exception, but none was thrown")
  with
  | ex -> ex  

[<Test>]
let ``Extracts a value from an option, throwing an appropriate error otherwise`` () = 
  test <@ Some 5 |> Option.unless "Expecting to be able to extract a value from a Some Option" = 5 @>

  let ex = 
    expectException (lazy (None |> Option.unless "Can't find"))

  test <@ ex.Message = "Can't find" @>


[<Test>]
let ``can convert to and from a Result`` () =
  test 
    <@
      Some 4 |> Option.toResult "didn't work" = Ok 4
      &&
      Some 4 |> Option.toResultWith (fun() -> failwith "shouldn't have run this thunk") = Ok 4

      &&

      None |> Option.toResult "didn't work" = Error "didn't work"
      &&
      None |> Option.toResultWith (fun() -> "didn't work") = Error "didn't work"

      &&

      Ok 4 |> Option.ofResult = Some 4
      &&
      Error "didn't work" |> Option.ofResult = None
    @>

[<Test>]
let ``can convert to and from a VOption`` () =
  test 
    <@
      Some 4 |> Option.toVOption = ValueSome 4
      &&
      None |> Option.toVOption = ValueNone

      &&

      ValueSome 4 |> Option.ofVOption = Some 4
      &&
      ValueNone |> Option.ofVOption = None
    @>
