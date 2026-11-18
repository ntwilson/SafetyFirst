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

// IDictionary DOESN'T EXPOSE ITS COMPARER, SO ONLY A CONCRETE Dictionary'S IS KEPT.
// FALLBACK IS STRUCTURAL, NOT Default, TO MATCH F# dict (E.G. ARRAY KEYS)
let private comparerOf (xs : IDictionary<'k, 'v>) : IEqualityComparer<'k> =
  match xs with
  | :? Dictionary<'k, 'v> as d -> d.Comparer
  | _ -> HashIdentity.Structural

/// <summary>
/// Builds a new collection whose elements are the results of applying the given
/// function to each of the elements of the collection.
/// The key passed to the function indicates the key of element being transformed.
/// Keeps the key comparer of <c>xs</c> if it's a <c>Dictionary</c>, otherwise compares keys structurally.
/// </summary>
let map fn (xs:IDictionary<_,_>) =
  xs.ToDictionary (
    keySelector = (fun (KeyValue (k, v)) -> k),
    elementSelector = (fun (KeyValue (k, v)) -> fn k v),
    comparer = comparerOf xs
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

// Add, NOT THE INDEXER, BECAUSE Add THROWS WHEN A NON-Dictionary COMPARER IS STRICTER THAN STRUCTURAL AND KEYS COLLAPSE
let private traverseResult (fn : 'k -> 'a -> Result<'b, 'e>) (xs : IDictionary<'k, 'a>) : Result<Dictionary<'k, 'b>, 'e> =
  let mapped = Dictionary<'k, 'b> (xs.Count, comparerOf xs)
  use bindings = xs.GetEnumerator ()
  let rec traverse' () =
    if bindings.MoveNext ()
    then
      let (KeyValue (key, value)) = bindings.Current
      match fn key value with
      | Ok y ->
        mapped.Add(key, y)
        traverse' ()
      | Error e -> Error e
    else Ok mapped

  traverse' ()

// Add, NOT THE INDEXER, BECAUSE Add THROWS WHEN A NON-Dictionary COMPARER IS STRICTER THAN STRUCTURAL AND KEYS COLLAPSE
let private partitionMapResult (fn : 'k -> 'a -> Result<'b, 'e>) (xs : IDictionary<'k, 'a>) : Dictionary<'k, 'b> * Dictionary<'k, 'e> =
  let comparer = comparerOf xs
  let oks = Dictionary<'k, 'b> (comparer)
  let errs = Dictionary<'k, 'e> (comparer)
  for KeyValue (key, value) in xs do
    match fn key value with
    | Ok y -> oks.Add(key, y)
    | Error e -> errs.Add(key, e)

  (oks, errs)

let private collectResult (results : IDictionary<_, Result<_, _>>) : Result<Dictionary<_, _>, NonEmptyDictionary<_, _>> =
  let oks, errs = partitionMapResult (fun _ r -> r) results
  if errs.Count = 0 then Ok oks else Error (NonEmpty (errs :> IDictionary<_, _>))

/// <summary>
/// Functions for dictionaries of Results, or for mapping dictionaries with Result-producing functions.
/// </summary>
module Result =
  /// <summary>
  /// Maps each binding with <c>fn</c>, returning Ok of all the mapped values, or the first Error in enumeration order.
  /// Bindings after the first Error are not evaluated.
  /// </summary>
  let traverse fn (xs : IDictionary<_, _>) = traverseResult fn xs

  /// <summary>
  /// Returns Ok of all the contained values, or the first Error in enumeration order.
  /// Bindings after the first Error are not evaluated.
  /// </summary>
  let sequence (results : IDictionary<_, Result<_, _>>) = traverse (fun _ r -> r) results

  /// <summary>
  /// Returns Ok of all the contained values if every Result is Ok, otherwise an Error of every contained error by key.
  /// </summary>
  let collect (results : IDictionary<_, Result<_, _>>) = collectResult results

  /// <summary>
  /// Maps each binding with <c>fn</c>, splitting the results into the Ok values and the Error values by key.
  /// </summary>
  let partitionMap fn (xs : IDictionary<_, _>) = partitionMapResult fn xs

  /// <summary>
  /// Splits the Results into the Ok values and the Error values by key.
  /// </summary>
  let partition (results : IDictionary<_, Result<_, _>>) = partitionMap (fun _ r -> r) results

/// <summary>
/// Functions for manipulating NonEmpty Dictionaries
/// </summary>
module NonEmpty =
  /// <summary>
  /// Functions for NonEmpty dictionaries of Results, or for mapping NonEmpty dictionaries with Result-producing functions.
  /// </summary>
  module Result =
    /// <summary>
    /// Maps each binding with <c>fn</c>, returning Ok of all the mapped values, or the first Error in enumeration order.
    /// Bindings after the first Error are not evaluated.
    /// </summary>
    let traverse fn (NonEmpty xs : NonEmptyDictionary<_, _>) : Result<NonEmptyDictionary<_, _>, _> =
      traverseResult fn xs |> Result.map (fun d -> NonEmpty (d :> IDictionary<_, _>))

    /// <summary>
    /// Returns Ok of all the contained values, or the first Error in enumeration order.
    /// Bindings after the first Error are not evaluated.
    /// </summary>
    let sequence (results : NonEmptyDictionary<_, Result<_, _>>) = traverse (fun _ r -> r) results

    /// <summary>
    /// Returns Ok of all the contained values if every Result is Ok, otherwise an Error of every contained error by key.
    /// </summary>
    let collect (NonEmpty results : NonEmptyDictionary<_, Result<_, _>>)
      : Result<NonEmptyDictionary<_, _>, NonEmptyDictionary<_, _>> =
      collectResult results |> Result.map (fun d -> NonEmpty (d :> IDictionary<_, _>))

    /// <summary>
    /// Maps each binding with <c>fn</c>, splitting the results into the Ok values and the Error values by key.
    /// </summary>
    let partitionMap fn (NonEmpty xs : NonEmptyDictionary<_, _>) = partitionMapResult fn xs

    /// <summary>
    /// Splits the Results into the Ok values and the Error values by key.
    /// </summary>
    let partition (results : NonEmptyDictionary<_, Result<_, _>>) = partitionMap (fun _ r -> r) results
