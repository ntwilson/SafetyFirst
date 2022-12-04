module SafetyFirst.Map

open SafetyFirst.ErrorTypes

/// <summary>
/// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
/// and an Error if not.
/// </summary>
[<CompiledName("findSafe_F#")>]
let find' key map = 
  match Map.tryFind key map with
  | Some value -> Ok value
  | None -> Error (mapFindErr key)

/// <summary>
/// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
/// and an Error if not.
/// </summary>
let inline findSafe key map = find' key map

let private duplicates xs = 
  [ for (x, count) in xs |> Seq.countBy fst do if count > 1 then yield x ]

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
[<CompiledName("ofListSafe_F#")>]
let ofList' xs = 
  let result = Map.ofList xs
  if List.length xs <> Map.count result
  then Error <| duplicateKeysErr (duplicates xs) 
  else Ok result

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
let inline ofListSafe xs = ofList' xs 

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns None if any duplicate keys were found.
/// </summary>
let inline tryOfList xs = ofList' xs |> Result.toOption

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
[<CompiledName("ofArraySafe_F#")>]
let ofArray' xs =
  let result = Map.ofArray xs
  if Array.length xs <> Map.count result
  then Error <| duplicateKeysErr (duplicates xs)
  else Ok result

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
let inline ofArraySafe xs = ofArray' xs

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns None if any duplicate keys were found.
/// </summary>
let inline tryOfArray xs = ofArray' xs |> Result.toOption

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
[<CompiledName("ofSeqSafe_F#")>]
let inline ofSeq' xs = ofArray' (Seq.toArray xs)

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
let inline ofSeqSafe xs = ofSeq' xs

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns None if any duplicate keys were found.
/// </summary>
let inline tryOfSeq xs = ofSeq' xs |> Result.toOption
