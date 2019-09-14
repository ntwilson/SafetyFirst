namespace SafetyFirst

open System.Collections.Generic
open SafetyFirst.Numbers

/// <summary>
/// An infinite sequence created by <c>InfiniteSeq.init</c>.
/// The functions in InfiniteSeq are all safe for use with infinite sequences.
/// Note that an InfiniteSeq is technically finite, with an upper bound supplied
/// at the time of creation. This upper bound represents a limit such that we can 
/// be sure that the application "hung" if the sequence produced that many elements.
/// This allows for safe usage of <c>InfiniteSeq</c> without needing to worry about
/// the application truly hanging in an infinite loop.
/// Note that this makes the InfiniteSeq type inappropriate for intentionally initiating
/// an infinite loop (e.g., with an <c>iter</c> function).  You might consider using a 
/// regular infinite seq with <c>Seq.initInfinite</c> if you're looking to initiate an 
/// infinite loop.
/// </summary>
type InfiniteSeq<'a> = private InfiniteSeq of seq<'a>

/// <summary>
/// Functions safe to use with InfiniteSeqs.  None of the functions in this module
/// hang indefinitely.
/// </summary>
module InfiniteSeq =

  type InfiniteSeqMaxElements = MaxElements of int

  type InfiniteSeqHung = InfiniteSeqHung of string
  let private hungErr _ = InfiniteSeqHung "Program execution hung.  This infinite sequence was allowed to evaluate elements for too long."

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive
  /// elements by calling the given function. The results of calling the function
  /// will not be saved, that is the function will be reapplied as necessary to
  /// regenerate the elements. The function is passed the index of the item being
  /// generated.
  /// Note that an InfiniteSeq is technically finite, with the upper bound supplied
  /// representing a limit such that we can be sure that the application "hung" if 
  /// the sequence produced that many elements.
  /// This allows for safe usage of <c>InfiniteSeq</c> without needing to worry about
  /// the application truly hanging in an infinite loop.
  /// </summary>
  let init (MaxElements maxElements) transform = 
    InfiniteSeq (Seq.initInfinite transform |> Seq.truncate maxElements)

  /// <summary>
  /// Computes the element at the specified index in the collection.
  /// </summary>
  let item' (NaturalInt i) (InfiniteSeq xs) = 
    Seq.item' i xs |> Result.mapError hungErr

  /// <summary>
  /// Computes the element at the specified index in the collection.
  /// </summary>
  let itemSafe i xs = item' i xs

  /// <summary>
  /// Applies the given function to each element of the seq. Return the seq comprised of the results <c>x</c> 
  /// for each element where the function returns <c>Some(x)</c>.
  /// The returned sequence may be passed between threads safely. However, individual IEnumerator 
  /// values generated from the returned sequence should not be accessed concurrently.
  /// </summary>
  let choose chooser (InfiniteSeq xs) = InfiniteSeq (Seq.choose chooser xs)

  /// <summary>
  /// Divides the input sequence into chunks of size at most <c>size</c>.
  /// Same as <c>Seq.chunkBySize</c>, but restricts the input to a PositiveInt.
  /// CAUTION: This function will THROW for a chunkSize <= 0
  /// </summary>
  let chunkBySizeUnsafe chunkSize xs = InfiniteSeq <| Seq.chunkBySize chunkSize xs

  /// <summary>
  /// Divides the input sequence into chunks of size at most <c>size</c>.
  /// Same as <c>Seq.chunkBySize</c>, but restricts the input to a PositiveInt
  /// </summary>
  let chunksOf chunkSize (InfiniteSeq xs) = InfiniteSeq <| Seq.chunksOf chunkSize xs

  /// <summary>
  /// Returns the first N elements of the sequence.  Note that this will happen
  /// eagerly to check for a hang.
  /// </summary>
  let take n (InfiniteSeq xs) = Seq.take' n xs |> Result.mapError hungErr

  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.  Note that this will
  /// happen eagerly to check for a hang.
  /// </summary>
  // let takeWhile predicate (InfiniteSeq xs) = 
  //   fseq (Seq.takeWhile predicate xs)

  /// <summary>
  /// Returns a sequence that (eagerly) skips N elements of the underlying sequence and then 
  /// yields the remaining elements of the sequence (the remaining sequence remains lazy).
  /// </summary>
  let skip n (InfiniteSeq xs) = InfiniteSeq (Seq.skipLenient n xs)

  /// <summary>
  /// Returns a sequence that, when iterated, skips elements of the underlying sequence while the
  /// given predicate returns True, and then yields the remaining elements of the sequence.
  /// </summary>
  let skipWhile predicate (InfiniteSeq xs) = InfiniteSeq (Seq.skipWhile predicate xs)
  
  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let head' (InfiniteSeq xs) = Seq.head' xs |> Result.mapError hungErr

  /// <summary>
  /// Returns a sequence that skips 1 element of the underlying sequence and then yields the
  /// remaining elements of the sequence.
  /// </summary>
  let tail' (InfiniteSeq xs) = Seq.tail' xs |> Result.map InfiniteSeq |> Result.mapError hungErr

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let uncons xs = 
    result { 
      let! h = head' xs 
      let! t = tail' xs
      return (h, t)
    }

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let map f (InfiniteSeq xs) = InfiniteSeq (Seq.map f xs)

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (InfiniteSeq xs) = InfiniteSeq (Seq.mapi f xs)

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// </summary>
  let map2 f (InfiniteSeq xs) (InfiniteSeq ys) = InfiniteSeq <| Seq.map2 f xs ys

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// </summary>
  let map2L f (InfiniteSeq xs) ys = Seq.map2 f xs ys

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// </summary>
  let map2R f xs (InfiniteSeq ys) = Seq.map2 f xs ys

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let filter f (InfiniteSeq xs) = InfiniteSeq (Seq.filter f xs)

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (InfiniteSeq xs) = InfiniteSeq (Seq.pairwise xs)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (InfiniteSeq xs) = InfiniteSeq (Seq.scan f initialState xs)
  
  /// <summary>
  /// Use this function with caution.  Will continue to search the InfiniteSeq
  /// until an element matching the predicate is found, no matter how long it takes.
  /// </summary>
  let find' predicate (InfiniteSeq xs) = Seq.find' predicate xs |> Result.mapError hungErr

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// </summary>
  let zip (InfiniteSeq xs) (InfiniteSeq ys) = InfiniteSeq <| Seq.zip xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// When one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let zipL (InfiniteSeq xs) ys = Seq.zip xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// When one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let zipR xs (InfiniteSeq ys) = Seq.zip xs ys

  /// <summary>
  /// Splits a sequence at every occurrence of an element satisfying <c>splitAfter</c>.
  /// The split occurs immediately after each element that satisfies <c>splitAfter</c>,
  /// and the element satisfying <c>splitAfter</c> will be included as the last element of 
  /// the sequence preceeding the split.
  /// For example:
  /// <code>
  /// IniniteSeq.split ((=) 100) (seq {1;2;3;100;100;4;100;5;6;...})
  ///   //returns ([[1;2;3;100];[100];[4;100];[5;6];...])
  /// </code>
  /// </summary>
  let split splitAfter xs = 
    uncons xs 
    |> Result.map (fun (head, InfiniteSeq tail) ->
      let nonEmpty = Seq.NonEmpty.create head tail
      InfiniteSeq (Seq.NonEmpty.split splitAfter nonEmpty)
      |> map (InfiniteSeq << seq))
    |> Result.mapError hungErr

  let private uncurry f (a, b) = f a b

  /// <summary>
  /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
  /// For example:
  /// <code>
  /// NonEmptySeq.splitPairwise (=) (seq { 0;1;1;2;3;4;4;4;5;...})
  ///   //returns seq { [0;1];[1;2;3;4];[4];[4;5];... }
  /// </code>
  /// </summary>
  let splitPairwise splitBetween xs =
    uncons xs
    |> Result.map (fun (head, InfiniteSeq tail) -> 
      let nonEmpty = Seq.NonEmpty.create head tail
      InfiniteSeq (Seq.NonEmpty.splitPairwise splitBetween nonEmpty)
      |> map (InfiniteSeq << seq))
    |> Result.mapError hungErr
