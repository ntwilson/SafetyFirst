module SafetyFirst.Specs.MapSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Specs.ResultFixtures

[<Test>]
let ``Errors when constructing a Map that contains duplicate keys`` () = 
  test 
    <@
      match  
        Map.ofSeq' 
          [
            (1, "this")
            (2, "map")
            (3, "has")
            (2, "duplicate")
            (8, "keys")
          ]
        with
      | Error _ -> true
      | _ -> false        

      &&

      Map.ofSeq'
        [
          (1, "this")
          (2, "map")
          (4, "is")
          (8, "fine")
        ]
        =
        Ok (Map.ofSeq
          [
            (1, "this")
            (2, "map")
            (4, "is")
            (8, "fine")
          ])
    @>

let always f _ = f

[<Test>]
let ``can zip two maps with the same keys together`` () =
  let mapWithValueSet1 = [ ('a', 1); ('b', 2); ('c', 3) ] |> Map.ofList
  let mapWithValueSet2 = [ ('a', 4); ('b', 5); ('c', 6) ] |> Map.ofList

  test <@ Map.map2' (always (+)) mapWithValueSet1 mapWithValueSet2 = Ok (Map [ ('a', 5); ('b', 7); ('c', 9) ]) @>
  test <@ Map.zip' mapWithValueSet1 mapWithValueSet2 = Ok (Map [ ('a', (1, 4)); ('b', (2, 5)); ('c', (3, 6)) ]) @>

[<Test>]
let ``returns an error if zipping two maps with different keys`` () =
  let mapWithValueSet1 = 
    Map [ ('a', 1); ('b', 2); ('c', 3) ]
  let mapWithValueSet1ButDifferentKeys = 
    Map [ ('z', 0); ('a', 1); ('b', 2); ('c', 3) ]

  test <@ Map.map2' (always (+)) mapWithValueSet1 mapWithValueSet1ButDifferentKeys |> Result.isError @>
  test <@ Map.zip' mapWithValueSet1 mapWithValueSet1ButDifferentKeys |> Result.isError @>


[<Test>]
let ``can choose elements from a map after applying a function which produces options.`` () =
  let sampleMap = Map.ofList [ ('a', 1); ('b', 2); ('c', 3); ('d', 4); ('e', 5) ]
  let expectedResults = Map.ofList [ ('b', 10); ('d', 20) ]

  let results =
    sampleMap
    |> Map.choose (fun key value -> if value % 2 = 0 then Some(value * 5) else None)

  test <@ expectedResults = results @>


[<Test>]
let ``can combine two maps with mismatched keys`` () =
  let left = Map [ for i in 1..5 -> (i, i) ]
  let right = Map [ for i in 3..7 -> (i, i) ]

  let combined =
    (left, right)
    ||> Map.choose2 (fun i l r ->
      if i < 3 then
        option {
          let! l = l
          let! r = r
          return l + r
        }
      else
        Some <| Option.defaultValue 0 l + Option.defaultValue 0 r)

  test
    <@
      combined = Map.ofList
        [
          for i in 3..5 do
            yield (i, 2 * i)
          for i in 6..7 do
            yield (i, i)
        ]
    @>

[<Test>]
let ``can zip many maps together with a computation expression`` () = 
  let mapWithValueSet1 = Map [ ('a', 1); ('b', 2); ('c', 3) ]
  let mapWithValueSet2 = Map [ ('a', 4); ('b', 5); ('c', 6) ]
  let mapWithValueSet3 = Map [ ('a', 7); ('b', 8); ('c', 9) ]
  let mapWithValueSet1ButDifferentKeys = 
    Map [ ('z', 0); ('a', 1); ('b', 2); ('c', 3) ]

  let ans =
    Map.zipper {
      let! v1 = mapWithValueSet1
      and! v2 = mapWithValueSet2
      and! v3 = mapWithValueSet3
      and! v4 = mapWithValueSet1ButDifferentKeys
      return v1 + v2 + v3 + v4
    }

  test <@ ans = Map [ ('a', 1 + 4 + 7 + 1); ('b', 2 + 5 + 8 + 2); ('c', 3 + 6 + 9 + 3) ] @>



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
