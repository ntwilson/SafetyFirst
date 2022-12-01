module SafetyFirst.Specs.SetSpec

open NUnit.Framework
open FsCheck
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec

[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 Set.maxElement'<int> Set.maxElement
  alwaysProduceSameOutput1 Set.minElement'<int> Set.minElement

[<Test>]
let ``Safe Set functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrows1 Set.maxElement'<int> Set.maxElement
  errorsWheneverThrows1 Set.minElement'<int> Set.minElement