module SafetyFirst.Specs.StringSpec

open NUnit.Framework
open FsCheck

open SafetyFirst
open SafetyFirst.Numbers

open SafetyFirst.Specs.SeqSpec.SafeByType

let initByInts = (fun count initializer -> String.initN count (NaturalInt.value >> initializer))

[<Test>]
let ``String functions that are safe by type behave like the base functions`` () =
  alwaysProduceSameOutput2  initByInts         (String.init << NaturalInt.value)
  alwaysProduceSameOutput2  String.replicateN  (String.replicate << NaturalInt.value)