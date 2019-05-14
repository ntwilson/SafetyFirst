namespace SafetyFirst

open System.Collections.Generic

open ResultDotNet.FSharp

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
/// An alias for <c>NonEmpty<'a seq, 'a></c>.
/// </summary>
type NonEmptySeq<'a> = NonEmpty<'a seq, 'a>

/// <summary>
/// An array constrained to be non-empty. 
/// An alias for <c>NonEmpty<'a[], 'a></c>.
/// </summary>
type NonEmptyArray<'a> = NonEmpty<'a[], 'a>

/// <summary>
/// A list constrained to be non-empty. 
/// An alias for <c>NonEmpty<'a list, 'a></c>.
/// </summary>
type NonEmptyList<'a> = NonEmpty<'a list, 'a>

/// <summary>
/// A set constrained to be non-empty. 
/// An alias for <c>NonEmpty<Set<'a>, 'a></c>.
/// </summary>
type NonEmptySet<'a when 'a : comparison> = NonEmpty<Set<'a>, 'a>

[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:#seq<'a>) = 
    if Seq.isEmpty xs
    then Empty
    else NotEmpty (NonEmpty xs)

  let (|NonEmpty|) (NonEmpty xs) = xs

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
