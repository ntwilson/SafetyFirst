module SafetyFirst.Specs.ResultTraversalSpec

open System.Collections.Generic

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

// THROWS ABOVE 2, SO A PASSING SHORT-CIRCUIT TEST PROVES LATER ELEMENTS WERE NEVER EVALUATED
let tenfoldUnlessTwo x : Result<int, string> =
  match x with
  | 2 -> Error "two"
  | x when x > 2 -> failwith "evaluated past the first Error"
  | x -> Ok (x * 10)

let tenfoldIfEven x : Result<int, string> =
  if x % 2 = 0 then Ok (x * 10) else Error (sprintf "%i is odd" x)

let withKey fn (key : string) x = fn x |> Result.mapError (sprintf "%s: %s" key)

let toPairs (d : #seq<KeyValuePair<_, _>>) = d |> Seq.map (fun (KeyValue (k, v)) -> (k, v)) |> Seq.toList
let both (oks, errs) = (toPairs oks, toPairs errs)
let nonEmptyPairs (NonEmpty d : NonEmptyDictionary<_, _>) = toPairs d

let okAndErr : Result<int, string> list = [Ok 1; Error "a"; Ok 2; Error "b"]
let allOk : Result<int, string> list = [Ok 1; Ok 2]

[<Test>]
let ``List.Result functions`` () =
  test <@ List.Result.traverse tenfoldUnlessTwo [1; 1] = Ok [10; 10] @>
  test <@ List.Result.traverse tenfoldUnlessTwo [1; 2; 3] = Error "two" @>
  test <@ List.Result.traverse tenfoldUnlessTwo [] = Ok [] @>
  test <@ List.Result.sequence okAndErr = Error "a" @>
  test <@ List.Result.sequence allOk = Ok [1; 2] @>
  test <@ List.Result.collect okAndErr = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ List.Result.collect allOk = Ok [1; 2] @>
  test <@ List.Result.partitionMap tenfoldIfEven [1; 2; 3; 4] = ([20; 40], ["1 is odd"; "3 is odd"]) @>
  test <@ List.Result.partition okAndErr = ([1; 2], ["a"; "b"]) @>

[<Test>]
let ``List.NonEmpty.Result functions`` () =
  test <@ List.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [1; 1]) = Ok (NonEmpty.assume [10; 10]) @>
  test <@ List.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [1; 2; 3]) = Error "two" @>
  test <@ List.NonEmpty.Result.sequence (NonEmpty.assume okAndErr) = Error "a" @>
  test <@ List.NonEmpty.Result.sequence (NonEmpty.assume allOk) = Ok (NonEmpty.assume [1; 2]) @>
  test <@ List.NonEmpty.Result.collect (NonEmpty.assume okAndErr) = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ List.NonEmpty.Result.collect (NonEmpty.assume allOk) = Ok (NonEmpty.assume [1; 2]) @>
  test <@ List.NonEmpty.Result.partitionMap tenfoldIfEven (NonEmpty.assume [1; 2; 3; 4]) = ([20; 40], ["1 is odd"; "3 is odd"]) @>
  test <@ List.NonEmpty.Result.partition (NonEmpty.assume okAndErr) = ([1; 2], ["a"; "b"]) @>

[<Test>]
let ``Array.Result functions`` () =
  let okAndErr = List.toArray okAndErr
  let allOk = List.toArray allOk

  test <@ Array.Result.traverse tenfoldUnlessTwo [|1; 1|] = Ok [|10; 10|] @>
  test <@ Array.Result.traverse tenfoldUnlessTwo [|1; 2; 3|] = Error "two" @>
  test <@ Array.Result.traverse tenfoldUnlessTwo [||] = Ok [||] @>
  test <@ Array.Result.sequence okAndErr = Error "a" @>
  test <@ Array.Result.sequence allOk = Ok [|1; 2|] @>
  test <@ Array.Result.collect okAndErr = Error (NonEmpty.assume [|"a"; "b"|]) @>
  test <@ Array.Result.collect allOk = Ok [|1; 2|] @>
  test <@ Array.Result.partitionMap tenfoldIfEven [|1; 2; 3; 4|] = ([|20; 40|], [|"1 is odd"; "3 is odd"|]) @>
  test <@ Array.Result.partition okAndErr = ([|1; 2|], [|"a"; "b"|]) @>

[<Test>]
let ``Array.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (List.toArray okAndErr)
  let allOk = NonEmpty.assume (List.toArray allOk)

  test <@ Array.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [|1; 1|]) = Ok (NonEmpty.assume [|10; 10|]) @>
  test <@ Array.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume [|1; 2; 3|]) = Error "two" @>
  test <@ Array.NonEmpty.Result.sequence okAndErr = Error "a" @>
  test <@ Array.NonEmpty.Result.sequence allOk = Ok (NonEmpty.assume [|1; 2|]) @>
  test <@ Array.NonEmpty.Result.collect okAndErr = Error (NonEmpty.assume [|"a"; "b"|]) @>
  test <@ Array.NonEmpty.Result.collect allOk = Ok (NonEmpty.assume [|1; 2|]) @>
  test <@ Array.NonEmpty.Result.partitionMap tenfoldIfEven (NonEmpty.assume [|1; 2; 3; 4|]) = ([|20; 40|], [|"1 is odd"; "3 is odd"|]) @>
  test <@ Array.NonEmpty.Result.partition okAndErr = ([|1; 2|], [|"a"; "b"|]) @>

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
  test <@ Set.Result.partitionMap tenfoldIfEven (set [1; 2; 3; 4]) = (set [20; 40], set ["1 is odd"; "3 is odd"]) @>
  test <@ Set.Result.partition okAndErr = (set [1; 2], set ["a"; "b"]) @>

[<Test>]
let ``Set.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (set okAndErr)
  let allOk = NonEmpty.assume (set allOk)

  test <@ Set.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume (set [1; 0])) = Ok (NonEmpty.assume (set [0; 10])) @>
  test <@ Set.NonEmpty.Result.traverse tenfoldUnlessTwo (NonEmpty.assume (set [1; 2; 3])) = Error "two" @>
  test <@ Set.NonEmpty.Result.sequence allOk = Ok (NonEmpty.assume (set [1; 2])) @>
  test <@ Set.NonEmpty.Result.collect okAndErr = Error (NonEmpty.assume ["a"; "b"]) @>
  test <@ Set.NonEmpty.Result.collect allOk = Ok (NonEmpty.assume (set [1; 2])) @>
  test <@ Set.NonEmpty.Result.partitionMap tenfoldIfEven (NonEmpty.assume (set [1; 2; 3; 4])) = (set [20; 40], set ["1 is odd"; "3 is odd"]) @>
  test <@ Set.NonEmpty.Result.partition okAndErr = (set [1; 2], set ["a"; "b"]) @>

let keyedOkAndErr : (string * Result<int, string>) list = [("w", Ok 1); ("x", Error "a"); ("y", Ok 2); ("z", Error "b")]
let keyedAllOk : (string * Result<int, string>) list = [("w", Ok 1); ("x", Ok 2)]
let keyedNumbers = [("w", 1); ("x", 2); ("y", 3); ("z", 4)]

[<Test>]
let ``Map.Result functions`` () =
  let okAndErr = Map.ofList keyedOkAndErr
  let allOk = Map.ofList keyedAllOk

  test <@ Map.Result.traverse (withKey tenfoldUnlessTwo) (Map.ofList [("a", 1); ("b", 1)]) = Ok (Map.ofList [("a", 10); ("b", 10)]) @>
  test <@ Map.Result.traverse (withKey tenfoldUnlessTwo) (Map.ofList keyedNumbers) = Error "x: two" @>
  test <@ Map.Result.traverse (withKey tenfoldUnlessTwo) Map.empty = Ok Map.empty @>
  test <@ Map.Result.sequence okAndErr = Error "a" @>
  test <@ Map.Result.sequence allOk = Ok (Map.ofList [("w", 1); ("x", 2)]) @>
  test <@ Map.Result.collect okAndErr = Error (NonEmpty.assume (Map.ofList [("x", "a"); ("z", "b")])) @>
  test <@ Map.Result.collect allOk = Ok (Map.ofList [("w", 1); ("x", 2)]) @>
  test
    <@
      Map.Result.partitionMap (withKey tenfoldIfEven) (Map.ofList keyedNumbers)
        = (Map.ofList [("x", 20); ("z", 40)], Map.ofList [("w", "w: 1 is odd"); ("y", "y: 3 is odd")])
    @>
  test <@ Map.Result.partition okAndErr = (Map.ofList [("w", 1); ("y", 2)], Map.ofList [("x", "a"); ("z", "b")]) @>

[<Test>]
let ``Map.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (Map.ofList keyedOkAndErr)
  let allOk = NonEmpty.assume (Map.ofList keyedAllOk)

  test
    <@
      Map.NonEmpty.Result.traverse (withKey tenfoldUnlessTwo) (NonEmpty.assume (Map.ofList [("a", 1); ("b", 1)]))
        = Ok (NonEmpty.assume (Map.ofList [("a", 10); ("b", 10)]))
    @>
  test <@ Map.NonEmpty.Result.traverse (withKey tenfoldUnlessTwo) (NonEmpty.assume (Map.ofList keyedNumbers)) = Error "x: two" @>
  test <@ Map.NonEmpty.Result.sequence okAndErr = Error "a" @>
  test <@ Map.NonEmpty.Result.sequence allOk = Ok (NonEmpty.assume (Map.ofList [("w", 1); ("x", 2)])) @>
  test <@ Map.NonEmpty.Result.collect okAndErr = Error (NonEmpty.assume (Map.ofList [("x", "a"); ("z", "b")])) @>
  test <@ Map.NonEmpty.Result.collect allOk = Ok (NonEmpty.assume (Map.ofList [("w", 1); ("x", 2)])) @>
  test
    <@
      Map.NonEmpty.Result.partitionMap (withKey tenfoldIfEven) (NonEmpty.assume (Map.ofList keyedNumbers))
        = (Map.ofList [("x", 20); ("z", 40)], Map.ofList [("w", "w: 1 is odd"); ("y", "y: 3 is odd")])
    @>
  test <@ Map.NonEmpty.Result.partition okAndErr = (Map.ofList [("w", 1); ("y", 2)], Map.ofList [("x", "a"); ("z", "b")]) @>

// Dictionaries compare by reference, so these compare their key/value pairs instead.
[<Test>]
let ``Dictionary.Result functions`` () =
  let okAndErr = dict keyedOkAndErr
  let allOk = dict keyedAllOk

  test <@ Dictionary.Result.traverse (withKey tenfoldUnlessTwo) (dict [("a", 1); ("b", 1)]) |> Result.map toPairs = Ok [("a", 10); ("b", 10)] @>
  test <@ Dictionary.Result.traverse (withKey tenfoldUnlessTwo) (dict keyedNumbers) |> Result.map toPairs = Error "x: two" @>
  test <@ Dictionary.Result.traverse (withKey tenfoldUnlessTwo) (dict []) |> Result.map toPairs = Ok [] @>
  test <@ Dictionary.Result.sequence okAndErr |> Result.map toPairs = Error "a" @>
  test <@ Dictionary.Result.sequence allOk |> Result.map toPairs = Ok [("w", 1); ("x", 2)] @>
  test
    <@
      Dictionary.Result.collect okAndErr |> Result.map toPairs |> Result.mapError (fun (NonEmpty errs) -> toPairs errs)
        = Error [("x", "a"); ("z", "b")]
    @>
  test <@ Dictionary.Result.collect allOk |> Result.map toPairs = Ok [("w", 1); ("x", 2)] @>
  test
    <@
      Dictionary.Result.partitionMap (withKey tenfoldIfEven) (dict keyedNumbers) |> both
        = ([("x", 20); ("z", 40)], [("w", "w: 1 is odd"); ("y", "y: 3 is odd")])
    @>
  test <@ Dictionary.Result.partition okAndErr |> both = ([("w", 1); ("y", 2)], [("x", "a"); ("z", "b")]) @>

[<Test>]
let ``Dictionary.NonEmpty.Result functions`` () =
  let okAndErr = NonEmpty.assume (dict keyedOkAndErr)
  let allOk = NonEmpty.assume (dict keyedAllOk)

  test
    <@
      Dictionary.NonEmpty.Result.traverse (withKey tenfoldUnlessTwo) (NonEmpty.assume (dict [("a", 1); ("b", 1)]))
      |> Result.map nonEmptyPairs = Ok [("a", 10); ("b", 10)]
    @>
  test
    <@
      Dictionary.NonEmpty.Result.traverse (withKey tenfoldUnlessTwo) (NonEmpty.assume (dict keyedNumbers))
      |> Result.map nonEmptyPairs = Error "x: two"
    @>
  test <@ Dictionary.NonEmpty.Result.sequence okAndErr |> Result.map nonEmptyPairs = Error "a" @>
  test <@ Dictionary.NonEmpty.Result.sequence allOk |> Result.map nonEmptyPairs = Ok [("w", 1); ("x", 2)] @>
  test
    <@
      Dictionary.NonEmpty.Result.collect okAndErr |> Result.map nonEmptyPairs |> Result.mapError nonEmptyPairs
        = Error [("x", "a"); ("z", "b")]
    @>
  test <@ Dictionary.NonEmpty.Result.collect allOk |> Result.map nonEmptyPairs = Ok [("w", 1); ("x", 2)] @>
  test
    <@
      Dictionary.NonEmpty.Result.partitionMap (withKey tenfoldIfEven) (NonEmpty.assume (dict keyedNumbers)) |> both
        = ([("x", 20); ("z", 40)], [("w", "w: 1 is odd"); ("y", "y: 3 is odd")])
    @>
  test <@ Dictionary.NonEmpty.Result.partition okAndErr |> both = ([("w", 1); ("y", 2)], [("x", "a"); ("z", "b")]) @>

[<Test>]
let ``Dictionary functions keep the input Dictionary's key comparer`` () =
  let caseInsensitive = Dictionary<string, int> (System.StringComparer.OrdinalIgnoreCase)
  caseInsensitive.["a"] <- 2

  let mapped = Dictionary.map (fun _ x -> x * 10) caseInsensitive
  let traversed = Dictionary.Result.traverse (withKey tenfoldIfEven) caseInsensitive
  let oks, errs = Dictionary.Result.partitionMap (withKey tenfoldIfEven) caseInsensitive

  test <@ mapped.ContainsKey "A" @>
  test <@ traversed |> Result.map (fun d -> d.ContainsKey "A") = Ok true @>
  test <@ oks.ContainsKey "A" && errs.Comparer = (System.StringComparer.OrdinalIgnoreCase :> IEqualityComparer<string>) @>
