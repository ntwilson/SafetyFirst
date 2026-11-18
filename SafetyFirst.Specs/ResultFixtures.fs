/// Shared inputs for the collection modules' Result specs.
module SafetyFirst.Specs.ResultFixtures

// THROWS ABOVE 2, SO A PASSING SHORT-CIRCUIT TEST PROVES LATER ELEMENTS WERE NEVER EVALUATED
let tenfoldUnlessTwo x : Result<int, string> =
  match x with
  | 2 -> Error "two"
  | x when x > 2 -> failwith "evaluated past the first Error"
  | x -> Ok (x * 10)

let tenfoldIfEven x : Result<int, string> =
  if x % 2 = 0 then Ok (x * 10) else Error (sprintf "%i is odd" x)

let withKey fn (key : string) x = fn x |> Result.mapError (sprintf "%s: %s" key)

let okAndErr : Result<int, string> list = [Ok 1; Error "a"; Ok 2; Error "b"]
let allOk : Result<int, string> list = [Ok 1; Ok 2]

let keyedOkAndErr : (string * Result<int, string>) list = [("w", Ok 1); ("x", Error "a"); ("y", Ok 2); ("z", Error "b")]
let keyedAllOk : (string * Result<int, string>) list = [("w", Ok 1); ("x", Ok 2)]
let keyedNumbers = [("w", 1); ("x", 2); ("y", 3); ("z", 4)]
