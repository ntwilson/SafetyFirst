module SafetyFirst.Specs.SetSpec

open NUnit.Framework
open FsCheck
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec
open SafetyFirst.Specs.ResultFixtures

[<Test>]
let ``Safe Seq functions always produce the same output as unsafe versions for all random inputs`` () =
  alwaysProduceSameOutput1 Set.maxElement'<int> Set.maxElement
  alwaysProduceSameOutput1 Set.minElement'<int> Set.minElement

[<Test>]
let ``Safe Set functions error whenever unsafe versions throw for all random inputs`` () =
  errorsWheneverThrows1 Set.maxElement'<int> Set.maxElement
  errorsWheneverThrows1 Set.minElement'<int> Set.minElement

[<Test>]
let ``Set.Result functions`` () =
  let okAndErr = set okAndErr
  let allOk = set allOk

  test <@ Set.Result.traverse tenfoldUnlessTwo (set [1; 0]) = Ok (set [0; 10]) @>
  test <@ Set.Result.traverse tenfoldUnlessTwo (set [1; 2; 3]) = Error "two" @>
  test <@ Set.Result.traverse tenfoldUnlessTwo Set.empty = Ok Set.empty @>
  test <@ Set.Result.sequence allOk = Ok (set [1; 2]) @>
  test <@ Set.Result.collect okAndErr = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ Set.Result.collect allOk = Ok (set [1; 2]) @>

[<Test>]
let ``Set.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (set okAndErr)
  let allOk = NonEmpty.assume (set allOk)

  test <@ Set.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume (set [1; 0])) = Ok (NonEmpty.assume (set [0; 10])) @>
  test <@ Set.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume (set [1; 2; 3])) = Error "two" @>
  test <@ Set.NonEmpty.Result.sequence allOk = Ok (NonEmpty.assume (set [1; 2])) @>
  test <@ Set.NonEmpty.Result.collect okAndErr = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ Set.NonEmpty.Result.collect allOk = Ok (NonEmpty.assume (set [1; 2])) @>
