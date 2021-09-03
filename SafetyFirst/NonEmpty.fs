namespace SafetyFirst

open System.Collections.Generic
open FSharpPlus

/// <summary>
/// A sequence that is constrained to have at least one element.
/// </summary>
[<Struct>]
type NonEmpty<'a, 'b when 'a :> 'b seq> = private NonEmpty of 'a with 

  interface IEnumerable<'b> with
    member this.GetEnumerator () = 
      let (NonEmpty xs) = this
      in (xs :> _ seq).GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (NonEmpty xs) = this
      in upcast ((xs :> _ seq).GetEnumerator ())

/// <summary>
/// A seq constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;'a seq, 'a&gt;</c>.
/// </summary>
type NonEmptySeq<'a> = NonEmpty<'a seq, 'a>

/// <summary>
/// An array constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;'a[], 'a&gt;</c>.
/// </summary>
type NonEmptyArray<'a> = NonEmpty<'a[], 'a>

/// <summary>
/// A list constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;'a list, 'a&gt;</c>.
/// </summary>
type NonEmptyList<'a> = NonEmpty<'a list, 'a>

/// <summary>
/// A set constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;Set&lt;'a&gt;, 'a&gt;</c>.
/// </summary>
type NonEmptySet<'a when 'a : comparison> = NonEmpty<Set<'a>, 'a>

/// <summary>
/// A map constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;Map&lt;'a, 'b&gt;, KeyValuePair&lt;'a, 'b&gt;&gt;</c>.
/// </summary>
type NonEmptyMap<'a, 'b when 'a : comparison> = NonEmpty<Map<'a, 'b>, KeyValuePair<'a, 'b>>

/// <summary>
/// An IDictionary constrained to be non-empty. 
/// An alias for <c>NonEmpty&lt;IDictionary&lt;'a, 'b&gt;, KeyValuePair&lt;'a, 'b&gt;&gt;</c>.
/// </summary>
type NonEmptyDictionary<'a, 'b when 'a : comparison> = NonEmpty<IDictionary<'a, 'b>, KeyValuePair<'a, 'b>>

[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:#seq<'a>) = 
    if Seq.isEmpty xs
    then Empty
    else NotEmpty (NonEmpty xs)

  let (|NonEmpty|) (NonEmpty xs) = xs

open NonEmptySeqMatcher

module NonEmpty = 
  /// <summary>
  /// Attempt to convert the given sequence to a NonEmpty sequence.  Returns None if the 
  /// given sequence is empty. 
  /// </summary>
  let verify (xs:#seq<_>) =
    match xs with
    | Empty -> None
    | NotEmpty result -> Some result 

  /// <summary>
  /// Attempt to convert the given sequence to a NonEmpty sequence.  Throws if the 
  /// given sequence is empty. 
  /// </summary>
  let assume (xs:#seq<_>) = 
    match xs with
    | Empty -> failwith "Expecting a sequence containing one or more values, but got an empty sequence"
    | NotEmpty result -> result


type NonEmpty<'a, 'b when 'a :> 'b seq> with

  // --- Functor ---
  
  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptyList can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty xs, fn) : NonEmptyList<_> = NonEmpty (List.map fn xs)
  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptyArray can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty xs, fn) : NonEmptyArray<_> = NonEmpty (Array.map fn xs)
  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptySet can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty xs, fn) : NonEmptySet<_> = NonEmpty (Set.map fn xs)
  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptyMap can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty xs, fn) : NonEmptyMap<_,_> = NonEmpty (Map.map (fun k v -> fn v) xs)
  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptyDictionary can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty (xs:IDictionary<_,_>), fn) : NonEmptyDictionary<_,_> = 
    NonEmpty (dict [ for (KeyValue (k, v)) in xs -> (k, fn v) ])

  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptyDictionary can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty (xs:Dictionary<_,_>), fn) : NonEmpty<Dictionary<_,_>, _> = 
    NonEmpty (Dictionary (dict [ for (KeyValue (k, v)) in xs -> (k, fn v) ]))

  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmptySeq can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty (xs:_ seq), fn) : NonEmptySeq<_> = NonEmpty (Seq.map fn xs)

  /// <summary>
  /// Map compatible with FSharpPlus, so a NonEmpty ResizeArray can be mapped via the |>> operator, for example
  /// </summary>
  static member Map (NonEmpty (xs:ResizeArray<_>), fn) : NonEmpty<ResizeArray<_>, _> = 
    NonEmpty (ResizeArray (Seq.map fn xs))


  // --- Semigroup ---
  
  /// <summary>
  /// Append compatible with FSharpPlus, so NonEmptyLists can be appended via the ++ operator, for example
  /// </summary>
  static member (+) (NonEmpty xs, NonEmpty ys) : NonEmptyList<_> = NonEmpty (xs @ ys)
  /// <summary>
  /// Append compatible with FSharpPlus, so NonEmptyArrays can be appended via the ++ operator, for example
  /// </summary>
  static member (+) (NonEmpty xs, NonEmpty ys) : NonEmptyArray<_> = NonEmpty (Array.append xs ys)
  /// <summary>
  /// Append compatible with FSharpPlus, so NonEmptySets can be appended via the ++ operator, for example
  /// </summary>
  static member (+) (NonEmpty xs, NonEmpty ys) : NonEmptySet<_> = NonEmpty (Set.union xs ys)

  /// <summary>
  /// Append compatible with FSharpPlus, so NonEmptySeqs can be appended via the ++ operator, for example
  /// </summary>
  static member (+) (NonEmpty (xs:_ seq), NonEmpty (ys:_ seq)) : NonEmptySeq<_> = NonEmpty (Seq.append xs ys)

  /// <summary>
  /// Append compatible with FSharpPlus, so NonEmpty ResizeArrays can be appended via the ++ operator, for example
  /// </summary>
  static member (+) (NonEmpty (xs:ResizeArray<_>), NonEmpty (ys:ResizeArray<_>)) : NonEmpty<ResizeArray<_>, _> = 
    NonEmpty (ResizeArray (Seq.append xs ys))

  /// <summary>
  /// Traverse compatible with FSharpPlus, so any NonEmpty sequence can be used in the <c>traverse</c> function.
  /// </summary>
  static member inline Traverse (xs, f) = 
    let xs = NonEmptySeqMatcher.(|NonEmpty|) xs
    in traverse f xs |>> NonEmpty.assume

  /// <summary>
  /// Sequence compatible with FSharpPlus, so any NonEmpty sequence can be used in the <c>sequence</c> function.
  /// </summary>
  static member inline Sequence xs = 
    let xs = NonEmptySeqMatcher.(|NonEmpty|) xs
    in sequence xs |>> NonEmpty.assume

  // We will not be including semigroup instances for Maps/Dictionaries, since the behavior can be unclear.
  // Possible behoviors when there is a key collision include left-biased (keep the value from the left map),
  // right-biased (keep the value from the right map) and unbiased (values must be semigroups, and you append the two values).
  // At this time, we don't wish to choose a particular behavior.
