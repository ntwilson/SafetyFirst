module SafetyFirst.Specs.ResultSpec 
open System
open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``should have structural equality`` () =
  test
    <@
      CSharp.Result.Ok 5 = CSharp.Result.Ok 5
      &&
      CSharp.Result.Error "didn't work" = CSharp.Result.Error "didn't work"
      &&
      CSharp.Result.Ok 5 <> CSharp.Result.Ok 10
      &&
      CSharp.Result.Error "didn't work" <> CSharp.Result.Error "hello world"
    @>

[<Test>]
let ``static functions behave the same as the members`` () =
  test 
    <@
      Ok 5 |> Result.map ((+) 3) = Ok 8
      &&
      Error "didn't work" |> Result.map ((+) 3) = Error "didn't work"
      &&
      Ok 5 |> Result.bind (fun i -> Ok (i + 3)) = Ok 8
      &&
      Error "didn't work" |> Result.bind (fun i -> Ok (i + 3)) = Error "didn't work"
    @>

[<Test>]
let ``can map two results`` () =
  (=!)
    (Result.map2 (fun i j -> (float i) + j)
      (Ok 3)
      (Ok 5.))
    (Ok 8.)

  (=!)
    (Result.map2 (fun i j -> (float i) + j)
      (Error "didn't work")
      (Ok 5.))
    (Error "didn't work")
      
  (=!)
    (Result.map2 (fun i j -> (float i) + j)
      (Ok 3)
      (Error "didn't work"))
    (Error "didn't work")

[<Test>]
let ``can bind two results`` () =
  (=!)
    (Result.bind2 (fun i j -> Ok ((float i) + j))
      (Ok 3)
      (Ok 5.))
    (Ok 8.)

  (=!)
    (Result.bind2 (fun i j -> Ok ((float i) + j))
      (Error "didn't work")
      (Ok 5.))
    (Error "didn't work")
      
  (=!)
    (Result.bind2 (fun i j -> Ok ((float i) + j))
      (Ok 3)
      (Error "didn't work"))
    (Error "didn't work")

[<Test>]
let ``can map three results`` () =
  (=!)
  <| Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
      (Ok 13)
      (Ok 37.)
      (Ok "code")
  <| Ok "1337code"

  (=!)
  <| Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
      (Error "didn't work")
      (Ok 37.)
      (Ok "code")
  <| Error "didn't work"
      
  (=!)
  <| Result.map3 (fun i j k -> i.ToString() + j.ToString() + k)
      (Ok 13)
      (Ok 37.)
      (Error "didn't work")
  <| Error "didn't work"
  
[<Test>]
let ``can bind three results`` () =
  (=!)
  <| Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
      (Ok 13)
      (Ok 37.)
      (Ok "code")
  <| Ok "1337code"

  (=!)
  <| Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
      (Error "didn't work")
      (Ok 37.)
      (Ok "code")
  <| Error "didn't work"
      
  (=!)
  <| Result.bind3 (fun i j k -> Ok (i.ToString() + j.ToString() + k))
      (Ok 13)
      (Ok 37.)
      (Error "didn't work")
  <| (Error "didn't work")
  
[<Test>]
let ``can map four results`` () =
  (=!)
  <| Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
      (Ok 13)
      (Ok 37.)
      (Ok "co")
      (Ok "de")
  <| (Ok "1337code")

  (=!)
  <| Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
      (Error "didn't work")
      (Ok 37.)
      (Ok "co")
      (Ok "de")
  <| (Error "didn't work")
      
  (=!)
  <| Result.map4 (fun i j k l -> i.ToString() + j.ToString() + k + l)
      (Ok 13)
      (Ok 37.)
      (Ok "co")
      (Error "didn't work")
  <| (Error "didn't work")
  
[<Test>]
let ``can bind four results`` () =
  (=!)
  <| Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
      (Ok 13)
      (Ok 37.)
      (Ok "co")
      (Ok "de")
  <| (Ok "1337code")

  (=!)
  <| Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
      (Error "didn't work")
      (Ok 37.)
      (Ok "co")
      (Ok "de")
  <| (Error "didn't work")
      
  (=!)
  <| Result.bind4 (fun i j k l -> Ok (i.ToString() + j.ToString() + k + l))
      (Ok 13)
      (Ok 37.)
      (Ok "co")
      (Error "didn't work")
  <| (Error "didn't work")
  
[<Test>]
let ``can bind N results as an array`` () =
  (=!)
  <| Result.bindAll ((String.concat " ") >> Ok)
      ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Ok)
  <| (Ok "Results are better than exceptions")

  (=!)
  <| Result.bindAll (fun args -> Ok (Seq.reduce (+) args))
      ([|Ok 0; Ok 1; Ok 2; Error "didn't work"; Ok 4|])
  <| (Error "didn't work")
      
[<Test>]
let ``can map N results as an array`` () =
  (=!)
  <| Result.mapAll (fun args -> String.concat " " args)
      ([|"Results";"are";"better";"than";"exceptions"|] |> Seq.map Ok)
  <| (Ok "Results are better than exceptions")

  (=!)
  <| Result.mapAll (fun args -> Seq.reduce (+) args)
      ([|Ok 0; Ok 1; Ok 2; Error "didn't work"; Ok 4|])
  <| (Error "didn't work")

[<Test>]
let ``can execute an action on just ok or on just error`` () =
  let mutable didRun = false
  (Ok 5) |> Result.ifOk (fun i -> (didRun <- true))
  test <@ didRun @>

  didRun <- false
  (Error "didn't work")|> Result.ifOk (fun i -> (didRun <- true))
  test <@ not didRun @>

  didRun <- false
  (Error "didn't work")|> Result.ifError (fun err -> (didRun <- true))
  test <@ didRun @>

  didRun <- false
  (Ok 5)|> Result.ifError (fun err -> (didRun <- true))
  test <@ not didRun @>

[<Test>]
let ``can use computation expressions to bind and map Results`` () =
  let tryToGetA = Ok 5
  let tryToGetB = Ok 10
  let tryToGetC = Error "didn't work"
  let add x y = x + y
  let add' x y = Ok (x + y)

  let resultA = 
    result {
      let! x = tryToGetA
      let! y = tryToGetB
      return add x y 
    }

  let resultB =   
    result {
      let! x = tryToGetA
      let! y = tryToGetC
      return add x y 
    }

  let resultC =   
    result {
      let! x = tryToGetA
      let y = 10
      return! add' x y 
    }

  test
    <@
      resultA = (Ok 15)
      &&
      resultB = (Error "didn't work")
      &&
      resultC = (Ok 15)
    @>

[<Test>]
let ``can use the applicative CE to combine errors together`` () = 
  let tryToGetA = Ok 5
  let tryToGetB = Ok 10
  let add x y = x + y
  let add' x y = Ok (x + y)

  let resultA : Result<_, string> = 
    result {
      let! x = tryToGetA
      and! y = tryToGetB
      return add x y 
    }

  test <@ resultA = (Ok 15) @>

  let resultB = 
    result { 
      let! x = Error [ "x failed" ]
      and! y = tryToGetA
      and! z = Error [ "z failed" ]
      let a = 20
      return a+x+y+z
    }

  test <@ resultB = Error ["x failed"; "z failed"] @>

  let resultC = 
    result {
      let! x = Error "x failed"

      let! y = tryToGetA
      and! z = Error "z failed"

      return x+y+z
    }

  test <@ resultC = Error "x failed" @>

  let resultC = 
    result {
      let! x = tryToGetA

      let! y = Error ["y failed"]
      and! z = Error ["z failed"]

      return x+y+z
    }

  test <@ resultC = Error ["y failed"; "z failed"] @>


type CustomError = CustomError of string

[<Test>]
let ``can still create non applicative result expressions on non semigroup errors but will error for applicatives`` () = 
  let resultA = 
    result { 
      let! x = Ok 5
      let! y = Error <| CustomError "Failed!"
      return x + y
    }

  test <@ resultA = Error (CustomError "Failed!") @>

  // let ``uncomment to observe failure to compile`` = 
  //   result { 
  //     let! x = Ok 5
  //     and! y = Error <| CustomError "Failed!"
  //     return x + y
  //   }



[<Test>]
let ``can use a default value for a failed result`` () =
  test
    <@
      Error "didn't work" |> Result.defaultValue 10 = 10
      &&
      Ok "yay" |> Result.defaultValue "it didn't work" = "yay"

      &&

      Error "didn't work" |> Result.defaultWith (fun _ -> 10) = 10
      &&
      Ok "yay" |> Result.defaultWith (fun _ -> "it didn't work") = "yay"
    @>


let expectException act =
  try 
    let (Lazy result) = act 
    raise (AssertionException "Expected an Exception to be thrown, but none was") 
  with  
  | ex -> ex

  
[<Test>]
let ```unless` will extract the value from a Result with a message`` () = 
  test 
    <@
      Ok "yay" 
      |> Result.unless "Expected to be able to extract a value from an Ok Result" 
        = "yay"
    @>

  let ex = 
    expectException (
      lazy (Error "Didn't work" |> Result.unless "Can't find"))

  test 
    <@ 
      ex.Message.Contains "Didn't work" 
      && 
      ex.Message.Contains "Can't find"
    @>

  match ex with
  | :? SafetyFirst.ResultExpectedException<string> as rex -> 
    test <@ rex.ErrorDetails = "Didn't work" @>
  | _ -> 
    raise (AssertionException (sprintf "Expected a ResultExpectedException, but got a %s" (ex.GetType().FullName)))

[<Test>]
let ```expect` will extract the value from a Result without a message`` () = 
  test <@ Ok "yay" |> Result.expect = "yay" @>

  let ex = 
    expectException (
      lazy (Error "Didn't work" |> Result.expect))

  test <@ ex.Message.Contains "Didn't work" @>
  match ex with
  | :? SafetyFirst.ResultExpectedException<string> as rex -> 
    test <@ rex.ErrorDetails = "Didn't work" @>
  | _ -> 
    raise (AssertionException (sprintf "Expected a ResultExpectedException, but got a %s" (ex.GetType().FullName)))

type SampleErr = Catchable of string
type NotCatchableErr = NotCatchable of string

[<Test>]
let ``expecting a result still lets you catch specific exceptions`` () =
  test 
    <@
      try 
          Error (Catchable "didn't work") |> Result.expect
          false  
        with  
        | :? ResultExpectedException<SampleErr> as ex -> true
      
        = true
    @>

  expectException
    (lazy 
      ( try Error (NotCatchable "didn't work") |> Result.expect with
        | :? ResultExpectedException<SampleErr> as ex -> false))
  |> ignore

[<Test>]
let ``can convert to and from an option`` () = 
  test 
    <@
      Some 4 |> Result.ofOption "didn't work" = Ok 4
      &&
      Some 4 |> Result.ofOptionWith (fun() -> failwith "shouldn't have run this thunk") = (Ok 4)

      &&

      None |> Result.ofOption "didn't work" = Error "didn't work"
      &&
      None |> Result.ofOptionWith (fun() -> "didn't work") = Error "didn't work"

      &&

      Ok 4 |> Result.toOption = Some 4
      &&
      Error "didn't work" |> Result.toOption = None
    @>


[<Test>]
let ``Can tell if a Result is Ok or Error`` () = 
  test
    <@
      Error 5 |> Result.isError = true
      &&
      Error 5 |> Result.isOk = false

      &&

      Ok 5 |> Result.isOk = true
      &&
      Ok 5 |> Result.isError = false
    @>

let flip f a b = f b a

[<Test>]
let ``collects sequences of Results into a Result of a sequence of values`` () =
  test
    <@
      Result.collect [Ok 1; Ok 2; Ok 3] |> Result.map Seq.toList = (Ok [1; 2; 3])
      &&
      Result.collect [Ok 1; Error "didn't"; Error "work"] |> Result.mapError Seq.toList = (Error ["didn't"; "work"])
    @>

  test 
    <@ 
      Result.traverse (flip Array.item' [|1..4|]) [1;2;3] = Ok [2;3;4]
      &&
      Result.traverse (flip Array.item' [|1..4|]) [1;5;2] |> Result.isError
    @>

  test 
    <@
      Result.sequence [Ok 1; Ok 2; Ok 3] = (Ok [1; 2; 3])
      &&
      Result.sequence [Ok 1; Error "didn't"; Error "work"] = Error "didn't"
      &&
      Result.sequence [] = Ok []
    @>

  let mutable x = 0
  let ans = 
    Result.sequence (seq { 
      for i in 1 .. 3 do 
        x <- x + 1
        yield Ok x 
      
      yield Error "failed in the middle"

      for i in 4 .. 6 do
        x <- x + 1
        yield Ok x
    })
    
  test <@ ans = Error "failed in the middle" && x = 3 @>

type PretendError = PretendError

[<Test>]
let ``is easy to add context to result errors`` () = 
  let x = Error PretendError
  let y = x |> Result.createContext "this error is totally pretend"
  let z = y |> Result.addContext "really, really pretend"  

  match z with
  | Error { Context = ctx } -> test <@ ctx = ["really, really pretend"; "this error is totally pretend"] @>
  | Ok _ -> failwith "should have been error"
