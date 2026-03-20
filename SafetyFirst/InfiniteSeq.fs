namespace SafetyFirst

#nowarn "44"

open System
open System.Collections.Generic
open SafetyFirst.Numbers

/// <summary>
/// An infinite sequence created by e.g., <c>InfiniteSeq.init</c>.
/// The functions in InfiniteSeq are all safe for use with infinite sequences.
/// </summary>
type InfiniteSeq<'a> = 
  private | InfiniteSeq of seq<'a>

  interface IEnumerable<'a> with
    member this.GetEnumerator() = 
      let (InfiniteSeq xs) = this
      xs.GetEnumerator()

  interface System.Collections.IEnumerable with
    member this.GetEnumerator() = 
      let (InfiniteSeq xs) = this
      (xs :> System.Collections.IEnumerable).GetEnumerator()

  

[<AutoOpen>]
module InfiniteSeqTypes = 
  type InfiniteSeqMaxElements = MaxElements of int
  type InfiniteSeqHung = InfiniteSeqHung of string

/// <summary>
/// Functions safe to use with InfiniteSeqs.
/// </summary>
module InfiniteSeq =
  let private hung = InfiniteSeqHung "Program execution hung.  This infinite sequence was allowed to evaluate elements for too long."
  let private protect f x = 
    try Ok (f x)
    with :? InfiniteSequenceEvaluationHung -> Error hung

  let private toLazyResults (xs: _ seq) = 
    seq {
      use mutable iter = xs.GetEnumerator()
      let mutable keepGoing = true
      while keepGoing do
        let nextElement = 
          protect (fun _ -> 
            iter.MoveNext() |> ignore
            iter.Current) ()

        match nextElement with
        | Error e -> 
          keepGoing <- false
          yield Error e
        | Ok v -> 
          yield Ok v
    }

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive
  /// elements by calling the given function. The results of calling the function
  /// will not be saved, that is the function will be reapplied as necessary to
  /// regenerate the elements. The function is passed the index of the item being
  /// generated.
  /// Note that an InfiniteSeq created with this function is technically finite, 
  /// with the upper bound supplied
  /// representing a limit such that we can be sure that the application "hung" if 
  /// the sequence produced that many elements. If the sequence produces more than the 
  /// specified maximum number of elements, an exception will be thrown.
  /// </summary>
  let init (MaxElements maxElements) transform = 
    InfiniteSeq (Seq.initInfinite transform |> Seq.isHungAfter maxElements)

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive
  /// elements by calling the given function. The results of calling the function
  /// will not be saved, that is the function will be reapplied as necessary to
  /// regenerate the elements. The function is passed the index of the item being
  /// generated.
  /// Note that an InfiniteSeq created with this function is technically finite, 
  /// with the upper bound supplied
  /// representing a limit such that we can be sure that the application "hung" if 
  /// the sequence produced that many elements. If the sequence produces more than the 
  /// specified maximum number of elements, an exception will be thrown.
  /// </summary>
  let initBounded (maxElements) transform = init (MaxElements maxElements) transform

  /// <summary>
  /// Generates a new infinite sequence by calling the given function. Unlike <c>InfiniteSeq.init</c>,
  /// this version does not set an upper bound on the number of elements, so the application can hang if
  /// a bug causes an infinite loop.
  /// </summary>
  let initUnbounded transform = InfiniteSeq (Seq.initInfinite transform)

  /// <summary>
  /// Assert that the given sequence is infinite (or bounded by <c>Seq.isHungAfter</c>). Note that there 
  /// is no possible runtime check to ensure that the sequence is actually infinite. Functions in this module
  /// can throw if used with a sequence that is not actually infinite.
  /// </summary>
  let assume xs = InfiniteSeq xs

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive
  /// elements by calling the given function. The results of calling the function
  /// will not be saved, that is the function will be reapplied as necessary to
  /// regenerate the elements. The function is passed the index of the item being generated.
  /// Note that an InfiniteSeq created with this function is technically finite, 
  /// with the upper bound supplied
  /// representing a limit such that we can be sure that the application "hung" if 
  /// the sequence produced that many elements.
  /// </summary>
  [<CompilerMessage(message="not for use from F# - Intended to be used from C# only", messageNumber=17333, IsHidden=true)>]
  [<Obsolete("Use Seq.isHungAfter instead of the InfiniteSeq module.")>]
  let Init maxElements transform = 
    InfiniteSeq (Seq.initInfinite transform |> Seq.isHungAfter maxElements)

  /// <summary>
  /// Returns a new sequence that contains the elements of the first sequence followed by the elements of the second sequence.
  /// </summary>
  let append (xs) (InfiniteSeq ys) = InfiniteSeq (Seq.append xs ys)

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true".
  /// </summary>
  let filter f (InfiniteSeq xs) = InfiniteSeq (Seq.filter f xs)

  /// <summary>
  /// Guard against hanging by providing an upper bound that represents a limit such that we can 
  /// be sure that the application "hung" if the sequence produced that many elements. If more than 
  /// <c>maxElements</c> elements are consumed, an exception is thrown. While this can be used with
  /// any InfiniteSeq, this function is mostly for use with
  /// unbounded infinite sequences (created with <c>initUnbounded</c> or <c>assume</c>).
  /// For example, you might not know what a proper upper bound is until after you filter an infinite sequence.
  /// If used on an already bounded InfiniteSeq, 
  /// it will apply a new bound _on top of_ the existing bound, but will not override the existing one.
  /// So <c>InfiniteSeq.initBounded 100 |> InfiniteSeq.isHungAfter 500 |> InfiniteSeq.take 200</c> will throw an exception,
  /// as will <c>InfiniteSeq.initBounded 500 |> InfiniteSeq.isHungAfter 100 |> InfiniteSeq.take 200</c>
  /// </summary>
  let isHungAfter maxElements (InfiniteSeq xs) = InfiniteSeq (Seq.isHungAfter maxElements xs)

  /// <summary>
  /// Computes the element at the specified index in the collection.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// This function can throw if the computation hung when called on an InfiniteSeq created with <c>init</c>.
  /// </summary>
  let item (NaturalInt i) (InfiniteSeq xs) = Seq.item i xs

  /// <summary>
  /// Computes the element at the specified index in the collection. 
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use item instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  [<CompiledName("itemSafe_F#")>]
  let item' i xs =
    protect (item i) xs

  /// <summary>
  /// Computes the element at the specified index in the collection.
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use item instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let itemSafe i xs = item' i xs

  /// <summary>
  /// Computes the element at the specified index in the collection.
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use item instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let tryItem i xs = item' i xs |> Result.toOption

  /// <summary>
  /// Returns the first N elements of the sequence.
  /// This function returns immediately because of lazy evaluation, but when iterating the result, 
  /// it can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// Iterating this function's result can throw if the computation hung when called on an InfiniteSeq created with <c>init</c>.
  /// </summary>
  let take n (InfiniteSeq xs) = Seq.take n xs

  /// <summary>
  /// Returns the first N elements of the sequence. Note that this will happen eagerly to check for a hang. 
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use take instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  [<CompiledName("takeSafe_F#")>]
  let take' n xs =
    protect (take n >> List.ofSeq >> Seq.ofList) xs

  /// <summary>
  /// Returns the first N elements of the sequence. Note that this will happen eagerly to check for a hang. 
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use take instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let takeSafe n xs = take' n xs

  /// <summary>
  /// Returns the first N elements of the sequence. Note that this will happen eagerly to check for a hang. 
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use take instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let tryTake n xs = take' n xs |> Result.toOption

  /// <summary>
  /// Lazily returns up to the first N elements of the sequence.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// </summary>
  [<Obsolete("Use take instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let truncate n (InfiniteSeq xs) = 
    toLazyResults xs |> Seq.truncate n


  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// Iterating the result of this function can throw if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// </summary>
  let takeWhile predicate (InfiniteSeq xs) =
    Seq.takeWhile predicate xs

  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.  Note that the resulting
  /// sequence is evaluated eagerly to ensure that a hang does not occur when iterated.
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<CompiledName("takeWhileSafe_F#")>]
  [<Obsolete("Use takeWhile instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let takeWhile' predicate (InfiniteSeq xs) =
    let xs = Seq.cache xs
    xs 
    |> protect (Seq.find (not << predicate))
    |> Result.map (fun _ -> Seq.takeWhile predicate xs)

  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.  Note that the resulting
  /// sequence is evaluated eagerly to ensure that a hang does not occur when iterated.  If you
  /// expect to possibly receive an infinite result from this function, consider using
  /// <c>takeWhileLazy</c> instead.
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use takeWhile instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let tryTakeWhile predicate xs = takeWhile' predicate xs |> Result.toOption


  /// <summary>
  /// Lazily returns elements of the underlying sequence while the given predicate returns True, and
  /// then returns no further elements. Note that reaching the end of the infinite sequence represents
  /// the application hanging, and we cannot preemptively detect a hang while executing lazily.  As such
  /// the possibility of a hang is deferred to each individual element, which will throw an exception
  /// if the sequence hung (produced too many elements).  If you are expecting a finite
  /// result and are able to eagerly evaluate up to the first element that doesn't pass the predicate,
  /// consider using <c>takeWhile'</c> instead, which is likely easier to consume.
  /// </summary>
  [<Obsolete("Use takeWhile instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let takeWhileLazy predicate (InfiniteSeq xs) = 
    toLazyResults xs 
    |> Seq.takeWhile (function
      | Ok v -> predicate v
      | Error _ -> true)

  /// <summary>
  /// Applies the given function to each element of the seq. Return the seq comprised of the results <c>x</c> 
  /// for each element where the function returns <c>Some(x)</c>.
  /// The returned sequence may be passed between threads safely. However, individual IEnumerator 
  /// values generated from the returned sequence should not be accessed concurrently.
  /// </summary>
  let choose chooser (InfiniteSeq xs) = InfiniteSeq (Seq.choose chooser xs)

  /// <summary>
  /// Divides the input sequence into chunks of size <c>chunkSize</c>.
  /// Each chunk is guaranteed to contain <c>chunkSize</c> elements.
  /// Same as <c>InfiniteSeq.chunkBySizeUnsafe</c>, but restricts the input to a PositiveInt.
  /// </summary>
  let chunksOf chunkSize (InfiniteSeq xs) : InfiniteSeq<NonEmptyArray<_>> = 
    Seq.chunksOf chunkSize xs
    |> InfiniteSeq 

  /// <summary>
  /// Divides the input sequence into chunks of size at most <c>size</c>.
  /// Each chunk is guaranteed to contain <c>chunkSize</c> elements.
  /// Same as <c>InfiniteSeq.chunksOf</c>, but allows a regular int as input.
  /// CAUTION: This function will THROW for a chunkSize &lt;= 0
  /// </summary>
  let chunkBySizeUnsafe chunkSize xs =
    chunksOf (PositiveInt.assume chunkSize) xs 

  /// <summary>
  /// Returns a sequence that skips N elements of the underlying sequence and then 
  /// yields the remaining elements of the sequence.
  /// </summary>
  let skip n (InfiniteSeq xs) = InfiniteSeq (Seq.skipLenient n xs)

  /// <summary>
  /// Returns a sequence that, when iterated, skips elements of the underlying sequence while the
  /// given predicate returns True, and then yields the remaining elements of the sequence.
  /// </summary>
  let skipWhile predicate (InfiniteSeq xs) = InfiniteSeq (Seq.skipWhile predicate xs)

  /// <summary>
  /// Returns a sequence that, when iterated, skips elements of the underlying sequence 
  /// up to and including the first element for which the given predicate returns True, 
  /// and then yields the remaining elements of the sequence.
  /// Like <c>skipWhile</c>, but with an inverted predicate and 
  /// also skips the element for which the predicate first returns True.
  /// </summary>
  let skipUntilIncluding predicate (InfiniteSeq xs) = InfiniteSeq (Seq.skipUntilIncluding predicate xs)

  /// <summary>
  /// Returns the first element of the sequence.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// This function can throw if the computation hung when called on an InfiniteSeq created with <c>init</c>.
  /// </summary>
  let head (InfiniteSeq xs) = Seq.head xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<CompiledName("headSafe_F#")>]
  [<Obsolete("Use head instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let head' xs = protect head xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use head instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let inline tryHead xs = head' xs |> Result.toOption

  /// <summary>
  /// Returns a sequence that skips 1 element of the underlying sequence and then yields the
  /// remaining elements of the sequence. 
  /// </summary>
  let tail xs = skip 1 xs

  /// <summary>
  /// Returns tuple of head element and tail of the sequence.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// This function can throw if the computation hung when called on an InfiniteSeq created with <c>init</c>.
  /// </summary>
  let uncons xs = head xs, tail xs

  /// <summary>
  /// Returns tuple of head element and tail of the sequence.
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<CompiledName("unconsSafe_F#")>]
  [<Obsolete("Use uncons instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let uncons' xs =
    result {
      let! h = head' xs
      let t = tail xs
      return (h, t)
    }

  /// <summary>
  /// Returns tuple of head element and tail of the sequence.
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use uncons instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let tryUncons xs = uncons' xs |> Result.toOption

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
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. Truncates the
  /// infinite sequence to the same length as the finite sequence. The resulting sequence
  /// is computed eagerly (though of course the elements of the infinite sequence that aren't
  /// needed are left lazy). 
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.map2 instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let map2L f (InfiniteSeq xs) (ys: _ fseq) =
    Seq.map2 f xs ys |> protect (List.ofSeq >> fseq)

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. Truncates the
  /// infinite sequence to the same length as the finite sequence. The resulting sequence
  /// is computed eagerly (though of course the elements of the infinite sequence that aren't
  /// needed are left lazy). 
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.map2 instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let inline tryMap2L f xs ys = map2L f xs ys |> Result.toOption

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. Truncates the
  /// infinite sequence to the same length as the finite sequence. The resulting sequence
  /// is computed eagerly (though of course the elements of the infinite sequence that aren't
  /// needed are left lazy). 
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.map2 instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let map2R f (xs: _ fseq) (InfiniteSeq ys) =
    Seq.map2 f xs ys |> protect (List.ofSeq >> fseq)

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. Truncates the
  /// infinite sequence to the same length as the finite sequence. The resulting sequence
  /// is computed eagerly (though of course the elements of the infinite sequence that aren't
  /// needed are left lazy). 
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.map2 instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let inline tryMap2R f xs ys = map2R f xs ys |> Result.toOption

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (InfiniteSeq xs) = InfiniteSeq (Seq.pairwise xs)
  
  /// <summary>
  /// Searches the sequence until an element matching the predicate is found.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// This function can throw if the computation hung when called on an InfiniteSeq created with <c>init</c>.
  /// </summary>
  let find predicate (InfiniteSeq xs) = Seq.find predicate xs

  /// <summary>
  /// Searches the sequence until an element matching the predicate is found.
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<CompiledName("findSafe_F#")>]
  [<Obsolete("Use find instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let find' predicate xs = protect (find predicate) xs

  /// <summary>
  /// Searches the sequence until an element matching the predicate is found.
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use find instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let inline tryFind predicate xs = find' predicate xs |> Result.toOption

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// </summary>
  let zip (InfiniteSeq xs) (InfiniteSeq ys) = InfiniteSeq <| Seq.zip xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs.
  /// Truncates the infinite sequence to the same length as the finite sequence.
  /// The resulting sequence is computed eagerly (though of course the elements
  /// of the infinite sequence that aren't needed are left lazy).
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.zip instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let zipL (InfiniteSeq xs) (ys: _ fseq) =
    Seq.zip xs ys |> protect (List.ofSeq >> fseq)

  /// <summary>
  /// Combines the two sequences into a list of pairs.
  /// Truncates the infinite sequence to the same length as the finite sequence.
  /// The resulting sequence is computed eagerly (though of course the elements
  /// of the infinite sequence that aren't needed are left lazy).
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.zip instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let tryZipL xs ys = zipL xs ys |> Result.toOption

  /// <summary>
  /// Combines the two sequences into a list of pairs.
  /// Truncates the infinite sequence to the same length as the finite sequence.
  /// The resulting sequence is computed eagerly (though of course the elements
  /// of the infinite sequence that aren't needed are left lazy).
  /// Returns an error if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.zip instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let zipR (xs: _ fseq) (InfiniteSeq ys) =
    Seq.zip xs ys |> protect (List.ofSeq >> fseq)

  /// <summary>
  /// Combines the two sequences into a list of pairs.
  /// Truncates the infinite sequence to the same length as the finite sequence.
  /// The resulting sequence is computed eagerly (though of course the elements
  /// of the infinite sequence that aren't needed are left lazy).
  /// Returns None if the sequence hung on a bounded InfiniteSeq created with <c>init</c>.
  /// This function can hang for an unbounded InfiniteSeq (created with <c>initUnbounded</c> or <c>assume</c>).
  /// </summary>
  [<Obsolete("Use Seq.zip instead. See the release notes at https://github.com/ntwilson/SafetyFirst/blob/main/ReleaseNotes.md")>]
  let inline tryZipR xs ys = zipR xs ys |> Result.toOption

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (InfiniteSeq xs) = InfiniteSeq (Seq.scan f initialState xs)


  /// <summary>
  /// Splits a sequence at every occurrence of an element satisfying <c>splitAfter</c>.
  /// The split occurs immediately after each element that satisfies <c>splitAfter</c>,
  /// and the element satisfying <c>splitAfter</c> will be included as the last element of 
  /// the sequence preceding the split.
  /// For example:
  /// <code>
  /// IniniteSeq.split ((=) 100) (seq {1;2;3;100;100;4;100;5;6;...})
  ///   //returns ([[1;2;3;100];[100];[4;100];[5;6];...])
  /// </code>
  /// </summary>
  let split splitAfter xs = 
    InfiniteSeq (Seq.split splitAfter xs)

  /// <summary>
  /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
  /// For example:
  /// <code>
  /// InfiniteSeq.splitPairwise (=) (seq { 0;1;1;2;3;4;4;4;5;...})
  ///   //returns seq { [0;1];[1;2;3;4];[4];[4;5];... }
  /// </code>
  /// </summary>
  let splitPairwise splitBetween (InfiniteSeq xs) =
    InfiniteSeq (Seq.splitPairwise splitBetween xs)
