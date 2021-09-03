namespace SafetyFirst

open System
open System.Collections
open System.Collections.Generic

open FSharpx.Collections

/// <summary>
/// A lazy sequence constrained to be finite in length.
/// </summary>
type FiniteSeq<[<EqualityConditionalOn; ComparisonConditionalOn>]'a> (xs : LazyList<'a>) = 
  //this implementation is modeled off of how array hashes are computed, here: 
  //https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/FSharp.Core/prim-types.fs
  //starting at line 1680
  let hashCode = 
    lazy (
      let inline hashCombine nr x y = (x <<< 1) + y + 631 * nr

      let first18 = Seq.truncate 18 (Seq.indexed xs)
      first18 |> Seq.fold (fun acc (i, x) -> hashCombine i acc (Unchecked.hash x)) 0 
    )

  let length =
    lazy (LazyList.length xs)

  new (xs:'a seq) = FiniteSeq (LazyList.ofSeq xs)

  member private this.HasCalculatedLength = length.IsValueCreated
  member private this.HasCalculatedHash = hashCode.IsValueCreated

  member this.Values = xs
  member this.Length = length.Value

  interface IEnumerable<'a> with
    member this.GetEnumerator () = (xs :> _ seq).GetEnumerator()
    member this.GetEnumerator () : IEnumerator = upcast (xs :> _ seq).GetEnumerator()      

  interface IComparable with
    member this.CompareTo x =
      
      let compareLengths (ys : FiniteSeq<'a>) =
        compare (this.Length) (ys.Length)

      let compareElements (ys : FiniteSeq<'a>) = 
        match 
            Seq.map2 Unchecked.compare xs ys 
            |> Seq.tryFind ((<>) 0)
          with
          | Some i -> i
          | None -> 0

      match x with
      | :? (FiniteSeq<'a>) as ys -> 
        match compareElements ys with
        | 0 -> compareLengths ys 
        | i -> i 
      | _ -> invalidArg "x" (sprintf "Can't compare a %s with a %s" (this.GetType().Name) (x.GetType().Name))

  override this.Equals x = 
    let compareElements (ys : FiniteSeq<'a>) = 
      Seq.forall2 Unchecked.equals xs ys 

    let compareLengths (ys : FiniteSeq<'a>) =
      compare (this.Length) (ys.Length)

    match x with
    | :? FiniteSeq<'a> as xs ->
      let lengthCompare = 
        if this.HasCalculatedLength && xs.HasCalculatedLength then xs.Length = this.Length else true
      let hashCompare = 
        if this.HasCalculatedHash && xs.HasCalculatedHash then Unchecked.hash xs = Unchecked.hash this else true

      LanguagePrimitives.PhysicalEquality this xs 
      ||
      (
        lengthCompare 
        && 
        hashCompare
        &&
        compareElements xs
        && 
        compareLengths xs = 0
      )
    | _ -> false

  // interface IEquatable<FiniteSeq<'a>> with
  //   member this.Equals x = this.Equals x

  override this.GetHashCode () = hashCode.Value

type FSeq<'a> = FiniteSeq<'a>

type 'a fseq = FiniteSeq<'a>

/// <summary>
/// A seq constrained to be finite and non-empty. 
/// An alias for <c>NonEmpty&lt;'a fseq, 'a&gt;</c>
/// </summary>
type NonEmptyFSeq<'a> = NonEmpty<'a fseq, 'a>

[<AutoOpen>]
module FSeqBuilder = 
  /// <summary>
  /// A lazy sequence constrained to be finite in length.  There is no possible runtime check
  /// for whether or not a seq is infinite, so this is more of an assertion of the programmer
  /// that this particular seq is finite.
  /// </summary>
  let fseq (xs:_ seq) : _ fseq = FiniteSeq xs
  let (|FiniteSeq|) (xs:_ fseq) = xs.Values
  let (|FSeq|) (xs:_ fseq) = xs.Values


