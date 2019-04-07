namespace SafetyFirst

open System.Runtime.CompilerServices
open System.Collections.Generic

[<Extension>]
module IDictionaryExtensions = 
  /// <summary>
  /// Lookup an element in the map, returning an Ok value if the element is in the domain of the map
  /// and an Error if not.
  /// </summary>
  [<Extension>]
  let FindSafe (map:IDictionary<_,_>, key) = Dictionary.find' key map
  

module Prelude = 
  /// <summary>
  /// Returns a new map made from the given bindings, provided all keys are unique.
  /// Returns an Error if any duplicate keys were found.
  /// </summary>
  let DictSafe keysAndValues = 
    Dictionary.ofSeq' keysAndValues

  /// <summary>
  /// Returns a new map made from the given bindings.
  /// If the bindings contain any duplicate keys, the latest value for each key is used.
  /// </summary>
  let DictLenient keysAndValues = 
    dict keysAndValues

