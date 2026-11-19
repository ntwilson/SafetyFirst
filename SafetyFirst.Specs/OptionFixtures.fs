/// Shared inputs for the collection modules' Option and ValueOption specs.
module SafetyFirst.Specs.OptionFixtures

open SafetyFirst
open SafetyFirst.Specs.ResultFixtures

let tenfoldUnlessTwoOpt x = tenfoldUnlessTwo x |> Option.ofResult
let tenfoldUnlessTwoVOpt x = tenfoldUnlessTwo x |> ValueOption.ofResult
let withKeyOpt fn (_ : string) x = fn x

let someAndNone = [Some 1; None; Some 2]
let allSome = [Some 1; Some 2]
let vsomeAndNone = [ValueSome 1; ValueNone; ValueSome 2]
let allVSome = [ValueSome 1; ValueSome 2]

let keyedSomeAndNone = [("w", Some 1); ("x", None); ("y", Some 2)]
let keyedAllSome = [("w", Some 1); ("x", Some 2)]
let keyedVSomeAndNone = [("w", ValueSome 1); ("x", ValueNone); ("y", ValueSome 2)]
let keyedAllVSome = [("w", ValueSome 1); ("x", ValueSome 2)]
