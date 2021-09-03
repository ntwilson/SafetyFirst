namespace SafetyFirst

open System.Collections.Generic

/// A sequence that is constrained to have at least one element.
[<Struct>]
type NonEmpty<'a, 'b when 'a :> 'b seq> = private NonEmpty of 'a with 

  interface IEnumerable<'b> with
    member this.GetEnumerator () = 
      let (NonEmpty xs) = this
      in (xs :> _ seq).GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (NonEmpty xs) = this
      in upcast ((xs :> _ seq).GetEnumerator ())

/// A seq constrained to be non-empty. 
/// An alias for `NonEmpty<'a seq, 'a>`.
type NonEmptySeq<'a> = NonEmpty<'a seq, 'a>

/// An array constrained to be non-empty. 
/// An alias for `NonEmpty<'a[], 'a>`.
type NonEmptyArray<'a> = NonEmpty<'a[], 'a>

/// A list constrained to be non-empty. 
/// An alias for `NonEmpty<'a list, 'a>`.
type NonEmptyList<'a> = NonEmpty<'a list, 'a>

/// A set constrained to be non-empty. 
/// An alias for `NonEmpty<Set<'a>, 'a>`.
type NonEmptySet<'a when 'a : comparison> = NonEmpty<Set<'a>, 'a>

[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:#seq<'a>) = 
    if Seq.isEmpty xs
    then Empty
    else NotEmpty (NonEmpty xs)

  let (|NonEmpty|) (NonEmpty xs) = xs

module NonEmpty = 
  /// Attempt to convert the given sequence to a NonEmpty sequence.  Returns None if the 
  /// given sequence is empty. 
  let verify (xs:#seq<_>) =
    match xs with
    | Empty -> None
    | NotEmpty result -> Some result 

  /// Attempt to convert the given sequence to a NonEmpty sequence.  Throws if the 
  /// given sequence is empty. 
  let assume (xs:#seq<_>) = 
    match xs with
    | Empty -> failwith "Expecting a sequence containing one or more values, but got an empty sequence"
    | NotEmpty result -> result
