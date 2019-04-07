module SafetyFirst.Dictionary

open System.Collections.Generic
open ResultDotNet.FSharp

open SafetyFirst.ErrorTypes

/// <summary>
/// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
/// and an Error if not.
/// </summary>
let find' key (map:IDictionary<_,_>) = 
  match map.TryGetValue key with
  | (true, value) -> Ok value
  | (false, _) -> Error (mapFindErr key)

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
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
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
/// Returns an Error if any duplicate keys were found.
/// </summary>
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
/// Returns an Error if any duplicate keys were found.
/// </summary>
let inline ofSeq' xs = ofArray' (Seq.toArray xs)

/// <summary>
/// Returns a new map made from the given bindings, provided all keys are unique.
/// Returns an Error if any duplicate keys were found.
/// </summary>
let inline ofSeqSafe xs = ofSeq' xs
