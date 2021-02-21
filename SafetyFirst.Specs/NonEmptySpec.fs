module SafetyFirst.Specs.NonEmptySpec

open NUnit.Framework
open Swensen.Unquote

open FSharpPlus
open SafetyFirst

[<Test>]
let ``can map and append non empty colletions via FSharpPlus`` () = 
  let (NonEmpty xs) = ((+) "hello ") <<| NonEmpty.assume ["world"; "SafetyFirst"]  
  test <@ xs = ["hello world"; "hello SafetyFirst"] @>


  let (NonEmpty ys) = (NonEmpty.assume [|1.0 .. 5.0|] |>> ((+) 2.0)) ++ (NonEmpty.assume [|7.0 .. 9.0|] |>> ((+) 1.0))
  test <@ ys = [| 3.0 .. 10.0|] @>


