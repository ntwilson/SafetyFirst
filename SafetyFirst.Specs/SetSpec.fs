module SafetyFirst.Specs.SetSpec

open NUnit.Framework
open FsCheck
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.SeqSpec
open SafetyFirst.Specs.ResultFixtures
open SafetyFirst.Specs.OptionFixtures

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

[<Test>]
let ``Set.Option functions`` () =
  test <@ Set.Option.traverse tenfoldUnlessTwoOpt (set [1; 0]) = Some (set [0; 10]) @>
  test <@ Set.Option.traverse tenfoldUnlessTwoOpt (set [1; 2; 3]) = None @>
  test <@ Set.Option.traverse tenfoldUnlessTwoOpt Set.empty = Some Set.empty @>
  test <@ Set.Option.sequence (set someAndNone) = None @>
  test <@ Set.Option.sequence (set allSome) = Some (set [1; 2]) @>

[<Test>]
let ``Set.NonEmpty.Option functions`` () =
  test <@ Set.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume (set [1; 0])) = Some (NonEmpty.assume (set [0; 10])) @>
  test <@ Set.NonEmpty.Option.traverse tenfoldUnlessTwoOpt (NonEmpty.assume (set [1; 2; 3])) = None @>
  test <@ Set.NonEmpty.Option.sequence (NonEmpty.assume (set someAndNone)) = None @>
  test <@ Set.NonEmpty.Option.sequence (NonEmpty.assume (set allSome)) = Some (NonEmpty.assume (set [1; 2])) @>

[<Test>]
let ``Set.ValueOption functions`` () =
  test <@ Set.ValueOption.traverse tenfoldUnlessTwoVOpt (set [1; 0]) = ValueSome (set [0; 10]) @>
  test <@ Set.ValueOption.traverse tenfoldUnlessTwoVOpt (set [1; 2; 3]) = ValueNone @>
  test <@ Set.ValueOption.traverse tenfoldUnlessTwoVOpt Set.empty = ValueSome Set.empty @>
  test <@ Set.ValueOption.sequence (set vsomeAndNone) = ValueNone @>
  test <@ Set.ValueOption.sequence (set allVSome) = ValueSome (set [1; 2]) @>

[<Test>]
let ``Set.NonEmpty.ValueOption functions`` () =
  test <@ Set.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume (set [1; 0])) = ValueSome (NonEmpty.assume (set [0; 10])) @>
  test <@ Set.NonEmpty.ValueOption.traverse tenfoldUnlessTwoVOpt (NonEmpty.assume (set [1; 2; 3])) = ValueNone @>
  test <@ Set.NonEmpty.ValueOption.sequence (NonEmpty.assume (set vsomeAndNone)) = ValueNone @>
  test <@ Set.NonEmpty.ValueOption.sequence (NonEmpty.assume (set allVSome)) = ValueSome (NonEmpty.assume (set [1; 2])) @>
