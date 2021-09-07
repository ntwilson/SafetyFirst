module SafetyFirst.Specs.NonEmptySpec

open NUnit.Framework
open Swensen.Unquote

open FSharpPlus
open SafetyFirst

[<Test>]
let ``can map and append non empty collections via FSharpPlus`` () = 
  let (NonEmpty xs) = ((+) "hello ") <<| NonEmpty.assume ["world"; "SafetyFirst"] 
  test <@ xs = ["hello world"; "hello SafetyFirst"] @>


  let (NonEmpty ys) = (NonEmpty.assume [|1.0 .. 5.0|] |>> ((+) 2.0)) ++ (NonEmpty.assume [|7.0 .. 9.0|] |>> ((+) 1.0))
  test <@ ys = [| 3.0 .. 10.0|] @>


[<Test>]
let ``can traverse non empty collections via FSharpPlus`` () = 
  let xs = NonEmpty.assume [Ok "hello"; Ok "world"] |> sequence
  test <@ xs = Ok (NonEmpty.assume [ "hello"; "world"]) @>

  let ys : Result<NonEmptyList<string>, string> = NonEmpty.assume [Ok "hello"; Error "FAILURE"] |> sequence
  test <@ ys = Error "FAILURE" @>


  let zs : Result<NonEmptyList<int>, string> = 
    NonEmpty.assume [1 .. 5] |> traverse (function 2 -> Error "It's a 2!" | i -> Ok i)
  test <@ zs = Error "It's a 2!" @>

  let oks = NonEmpty.assume [1 .. 5] |> traverse Some
  test <@ oks = Some (NonEmpty.assume [1 .. 5]) @>
