namespace SafetyFirst

open System.Collections.Generic

open FSharpx.Collections

/// A lazily evaluated sequence that is constrained to have at least one element.
type NonEmptySeq<'a when 'a : comparison> = 
  private
    | NonEmptySeq of FiniteSeq<'a>

  interface IEnumerable<'a> with
    member this.GetEnumerator () = 
      let (NonEmptySeq xs) = this
      in (xs :> _ seq).GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (NonEmptySeq xs) = this
      in upcast ((xs :> _ seq).GetEnumerator ())
  
[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:_ seq) = 
    let xs =
      match xs with
      | :? FiniteSeq<_> as a -> a
      | _ -> fseq xs
      
    if FSeq.isEmpty xs
    then Empty
    else NotEmpty (NonEmptySeq xs)

module NonEmptySeq =
  /// Creates a new NonEmptySeq with the provided head and tail.  
  /// The tail is constrained to be finite.  If the tail is infinite,
  /// create an InfiniteSeq instead of a NonEmptySeq.
  let create head (FSeq tail) = NonEmptySeq (FiniteSeq (LazyList.cons head tail))
  
  /// Returns the first element of the sequence.
  let head (NonEmptySeq xs) = Seq.head xs
  
  /// Returns the sequence after removing the first element.
  let tail (NonEmptySeq xs) = FiniteSeq.ofSeq (Seq.tail xs)
  
  /// Returns the tuple of the sequence's head and tail
  let uncons xs = (head xs, tail xs)
  
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.
  let reduce f (NonEmptySeq xs) = Seq.reduce f xs

  /// Returns the length of the sequence.
  let length (NonEmptySeq xs) = FiniteSeq.length xs
  
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  let fold f initialState (NonEmptySeq xs) = FiniteSeq.fold f initialState xs

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  let map f (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.map f xs)
  
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  let mapi f (NonEmptySeq xs) =
    NonEmptySeq (FiniteSeq.mapi f xs)
  
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  let map2 f (NonEmptySeq xs) (NonEmptySeq ys) =
    NonEmptySeq (FSeq.map2 f xs ys)

  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  let filter f (NonEmptySeq xs) = FiniteSeq.filter f xs

  /// Wraps the two given enumerations as a single concatenated enumeration.
  let append xs (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.append xs ys)

  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  let concat (NonEmptySeq xs : NonEmptySeq<NonEmptySeq<'a>>) = 
    NonEmptySeq (xs |> FiniteSeq.map (fun (NonEmptySeq x) -> x) |> FiniteSeq.concat)

  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  let drop n (NonEmptySeq xs) = FiniteSeq.drop n xs

  /// Returns a new sequence with the elements in reverse order.
  let rev (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.rev xs)

  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  let scan f initialState (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.scan f initialState xs)

  /// Builds an array from the given collection.
  let toArray (NonEmptySeq xs) = FiniteSeq.toArray xs

  /// Builds a List from the given collection.
  let toList (NonEmptySeq xs) = FiniteSeq.toList xs

  /// Views the given NonEmptySeq as a sequence.
  let toSeq (NonEmptySeq xs) : _ seq = upcast xs 

  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  let tryFind predicate (NonEmptySeq xs) = FiniteSeq.tryFind predicate xs

  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  let trySkip n (NonEmptySeq xs) = FiniteSeq.trySkip n xs

  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let tryTake n (NonEmptySeq xs) = FiniteSeq.tryTake n xs

  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths
  let tryZip (NonEmptySeq xs) (NonEmptySeq ys) = Option.map NonEmptySeq (FiniteSeq.tryZip xs ys)

  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  let zip (NonEmptySeq xs) (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.zip xs ys)
  
