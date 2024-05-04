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

/// <summary>
/// Returns the keys of the map as a set.
/// </summary>
let keys map =
  map |> Map.toSeq |> Seq.map fst |> Set.ofSeq

/// <summary>
/// Combines two maps using the given function. 
/// If a key is present in one map but not the other, the function is called with None for the missing value.
/// If the function returns None, the key is not included in the result.
/// </summary>
let choose2 (fn: 'key -> 'left option -> 'right option -> 'combined option) left right =
  let allKeys = keys left + keys right

  Map.ofList
    [
      for key in allKeys do
        match fn key (Map.tryFind key left) (Map.tryFind key right) with
        | Some combined -> yield (key, combined)
        | None -> ()
    ]

///<summary>
/// Applies the given function to each element of the map.
/// Returns the map comprised of the results for each element where the function returns Some.
///<summary/>
let choose (selector: 'Key -> 'T -> 'U option) map =
  map
  |> Map.map selector
  |> Map.filter (fun key value -> value |> Option.isSome)
  |> Map.map (fun _ value -> Option.get value)

/// <summary>
/// Builds a new map whose elements are the results of applying the given function
/// to the corresponding elements of the two maps. Any keys that are only in one of the two maps are ignored.
/// </summary>
let map2Intersection (fn: 'key -> 'left -> 'right -> 'combined) left right =
  choose2 (fun key left right -> 
    match left, right with
    | Some l, Some r -> Some (fn key l r)
    | _ -> None) left right

/// <summary>
/// Combines the two maps into a map of pairs. Any keys that are only in one of the two maps are ignored.
/// </summary>
let zipIntersection left right =
  map2Intersection (fun key l r -> (l, r)) left right

/// <summary>
/// Builds a new map whose elements are the results of applying the given function
/// to the corresponding elements of the two maps. Returns a MismatchingKeys error if 
/// any keys are only in one of the two maps.
/// </summary>
let map2Safe (fn: 'key -> 'left -> 'right -> 'combined) left right =
  let leftKeys = keys left
  let rightKeys = keys right

  if leftKeys <> rightKeys
  then Error <| mismatchingKeysErr "map2" 
  else Ok (map2Intersection fn left right)

/// <summary>
/// Builds a new map whose elements are the results of applying the given function
/// to the corresponding elements of the two maps. Returns a MismatchingKeys error if 
/// any keys are only in one of the two maps.
/// </summary>
let inline map2' fn left right = map2Safe fn left right

/// <summary>
/// Builds a new map whose elements are the results of applying the given function
/// to the corresponding elements of the two maps. Returns None if 
/// any keys are only in one of the two maps.
/// </summary>
let tryMap2 (fn: 'key -> 'left -> 'right -> 'combined) left right =
  map2Safe fn left right |> Result.toOption

/// <summary>
/// Combines the two maps into a map of pairs. Returns a MismatchingKeys error if 
/// any keys are only in one of the two maps.
/// </summary>
let zipSafe left right = map2Safe (fun key l r -> (l, r)) left right

/// <summary>
/// Combines the two maps into a map of pairs. Returns a MismatchingKeys error if 
/// any keys are only in one of the two maps.
/// </summary>
let inline zip' left right = zipSafe left right

/// <summary>
/// Combines the two maps into a map of pairs. Returns None if 
/// any keys are only in one of the two maps.
/// </summary>
let tryZip left right = zipSafe left right |> Result.toOption

type ZipperExpression() =
  member this.MergeSources(t1, t2) = zipIntersection t1 t2

  member this.BindReturn(x, f) = Map.map (fun key v -> f v) x

/// A zipper computation expression to zip any number of Maps together.
/// It zips according to zipIntersection, dropping any keys that are not present in all of the maps being zipped.
let zipper = new ZipperExpression()

