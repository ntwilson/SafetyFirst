module SafetyFirst.Specs.DictionarySpec

open System.Collections.Generic

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.ResultFixtures

// DICTIONARIES COMPARE BY REFERENCE, SO THESE TESTS COMPARE KEY/VALUE PAIRS INSTEAD
let toPairs (d : #seq<KeyValuePair<_, _>>) = d |> Seq.map (fun (KeyValue (k, v)) -> (k, v)) |> Seq.toList
let both (oks, errs) = (toPairs oks, toPairs errs)
let nonEmptyPairs (NonEmpty d : NonEmptyDictionary<_, _>) = toPairs d

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

[<Test>]
let ``Dictionary functions compare keys of a non-Dictionary structurally`` () =
  let arrayKeyed = dict [ ([| 1 |], 2) ]
  let alwaysOk _ x : Result<int, string> = Ok x

  let mapped = Dictionary.map (fun _ x -> x * 10) arrayKeyed
  let traversed = Dictionary.Result.traverse alwaysOk arrayKeyed
  let oks, _ = Dictionary.Result.partitionMap alwaysOk arrayKeyed

  // A FRESH ARRAY, SO REFERENCE EQUALITY WOULD MISS IT
  test <@ mapped.ContainsKey [| 1 |] @>
  test <@ traversed |> Result.map (fun d -> d.ContainsKey [| 1 |]) = Ok true @>
  test <@ oks.ContainsKey [| 1 |] @>
