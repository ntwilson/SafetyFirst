module SafetyFirst.Dictionary

open System.Collections.Generic
open System.Linq

open SafetyFirst.ErrorTypes

/// <summary>
/// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
/// and an Error if not.
/// </summary>
[<CompiledName("findSafe_F#")>]
let find' key (map:IDictionary<_,_>) = 
  match map.TryGetValue key with
  | (true, value) -> Ok value
  | (false, _) -> Error (mapFindErr key)

/// <summary>
/// Lookup an element in the map, returning an value if the element is in the domain of the map
/// and None if not.
/// </summary>
let tryFind key (map:IDictionary<_,_>) = 
  match map.TryGetValue key with
  | (true, value) -> Some value
  | (false, _) -> None

/// <summary>
/// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
/// and an Error if not.
/// </summary>
let inline findSafe key map = find' key map

let private duplicates xs = 
  [ for (x, count) in Seq.countBy fst xs do if count > 1 then yield x ]

/// <summary>
/// The number of bindings in the dictionary.
/// </summary>
let count (map:IDictionary<_,_>) = map.Count

/// <summary>
/// A collection containing the keys in the dictionary.
/// </summary>
let keys (map:IDictionary<_,_>) = map.Keys

/// <summary>
/// A collection containing the values in the dictionary.
/// </summary>
let values (map:IDictionary<_,_>) = map.Values

/// <summary>
/// Tests if an element is in the domain of the dictionary.
/// </summary>
let containsKey key (map:IDictionary<_,_>) = map.ContainsKey key

/// <summary>
/// Builds a new collection whose elements are the results of applying the given 
/// function to each of the elements of the collection. 
/// The key passed to the function indicates the key of element being transformed.
/// </summary>
let map fn (xs:IDictionary<_,_>) = 
  xs.ToDictionary (
    keySelector = (fun (KeyValue (k, v)) -> k),
    elementSelector = (fun (KeyValue (k, v)) -> fn k v)
  )

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
[<CompiledName("ofListSafe_F#")>]
let ofList' xs = 
  let result = dict xs
  if List.length xs <> count result
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
  let result = dict xs
  if Array.length xs <> count result
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
