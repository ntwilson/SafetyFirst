module SafetyFirst.Specs.ValueOptionSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

let x = ()

[<Test>]
let ``Converts NaNs to ValueNones, but any other float to ValueSome`` () =
  test
    <@
      ValueOption.ofFloat nan = ValueNone
      &&
      ValueOption.ofFloat 5. = ValueSome 5.
    @>

[<Test>]
let ``Collects multiple Options such that any ValueNone will make the whole thing ValueNone`` () =
  test
    <@
      ValueOption.collect [ ValueSome 1; ValueSome 2; ValueSome 3; ValueSome 4 ] |> ValueOption.map Seq.toList = ValueSome [1 .. 4]
      &&
      ValueOption.collect [ ValueNone; ValueSome 2; ValueSome 3; ValueSome 4 ] = ValueNone
      &&
      ValueOption.collect [ ValueSome 1; ValueNone; ValueSome 3; ValueSome 4 ] = ValueNone
      &&
      ValueOption.collect [ ValueSome 1; ValueSome 2; ValueSome 3; ValueNone ] = ValueNone      
    @>
  
let expectException act = 
  try
    let (Lazy value) = act 
    raise (AssertionException "Expecting an Exception, but ValueNone was thrown")
  with
  | ex -> ex  

[<Test>]
let ``Extracts a value from an option, throwing an appropriate error otherwise`` () = 
  test <@ ValueSome 5 |> ValueOption.unless "Expecting to be able to extract a value from a ValueSome ValueOption" = 5 @>

  let ex = 
    expectException (lazy (ValueNone |> ValueOption.unless "Can't find"))

  test <@ ex.Message = "Can't find" @>


[<Test>]
let ``can convert to and from a Result`` () =
  test 
    <@
      ValueSome 4 |> ValueOption.toResult "didn't work" = Ok 4
      &&
      ValueSome 4 |> ValueOption.toResultWith (fun() -> failwith "shouldn't have run this thunk") = Ok 4

      &&

      ValueNone |> ValueOption.toResult "didn't work" = Error "didn't work"
      &&
      ValueNone |> ValueOption.toResultWith (fun() -> "didn't work") = Error "didn't work"

      &&

      Ok 4 |> ValueOption.ofResult = ValueSome 4
      &&
      Error "didn't work" |> ValueOption.ofResult = ValueNone
    @>

[<Test>]
let ``can convert to and from an Option`` () =
  test 
    <@
      Some 4 |> ValueOption.ofOption = ValueSome 4
      &&
      None |> ValueOption.ofOption = ValueNone

      &&

      ValueSome 4 |> ValueOption.toOption = Some 4
      &&
      ValueNone |> ValueOption.toOption = None
    @>
