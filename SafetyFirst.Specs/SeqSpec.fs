module SafetyFirst.Specs.SeqSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``first test`` =
  test <@ Seq.head' [1 .. 3] = Ok 1 @>
