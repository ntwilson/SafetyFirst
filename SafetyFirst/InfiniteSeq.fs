namespace SafetyFirst

open System.Collections.Generic

/// An infinite sequence created by <c>InfiniteSeq.init</c>.
/// The functions in InfiniteSeq are all safe for use with infinite sequences
type InfiniteSeq<'a> = 
  private 
    | InfiniteSeq of seq<'a>

  interface IEnumerable<'a> with
    member this.GetEnumerator () = 
      let (InfiniteSeq xs) = this
      in xs.GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (InfiniteSeq xs) = this
      in upcast (xs.GetEnumerator ())

/// Functions safe to use with InfiniteSeq's.  None of the functions in this module
/// hang indefinitely for typical infinite sequences, though some of them could hang indefinitely
/// for some infinite sequences (e.g., using 
/// <c>InfiniteSeq.filter ((<>) 0) (InfiniteSeq.init (fun _ -> 0))</c> as a source
/// would cause many of the functions to hang indefinitely).
module InfiniteSeq =
  /// Generates a new sequence which, when iterated, will return successive
  /// elements by calling the given function. The results of calling the function
  /// will not be saved, that is the function will be reapplied as necessary to
  /// regenerate the elements. The function is passed the index of the item being
  /// generated.
  let init transform = InfiniteSeq (Seq.initInfinite transform)

  /// Returns the first N elements of the sequence.
  let take n (InfiniteSeq xs) = Seq.take n xs

  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.
  let takeWhile predicate (InfiniteSeq xs) = Seq.takeWhile predicate xs

  /// Returns a sequence that skips N elements of the underlying sequence and then yields the
  /// remaining elements of the sequence.
  let skip n (InfiniteSeq xs) = InfiniteSeq (Seq.skip n xs)
  
  /// Returns the first element of the sequence.
  let head (InfiniteSeq xs) = Seq.head xs

  /// Returns a sequence that skips 1 element of the underlying sequence and then yields the
  /// remaining elements of the sequence.
  let tail (InfiniteSeq xs) = InfiniteSeq (Seq.tail xs)

  /// O(1). Returns tuple of head element and tail of the list.
  let uncons xs = (head xs, tail xs)

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  let map f (InfiniteSeq xs) = InfiniteSeq (Seq.map f xs)

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  let mapi f (InfiniteSeq xs) = InfiniteSeq (Seq.mapi f xs)

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  let map2 f xs (InfiniteSeq ys) = Seq.map2 f xs ys

  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  let filter f (InfiniteSeq xs) = InfiniteSeq (Seq.filter f xs)

  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  let scan f initialState (InfiniteSeq xs) = InfiniteSeq (Seq.scan f initialState xs)
  
  /// Use this function with caution.  Will continue to search the InfiniteSeq
  /// until an element matching the predicate is found, no matter how long it takes.
  let find predicate (InfiniteSeq xs) = Seq.find predicate xs

  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  let zip xs (InfiniteSeq ys) = Seq.zip xs ys
