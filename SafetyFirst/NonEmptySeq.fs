namespace SafetyFirst

open System.Collections.Generic

open SafetyFirst.FSharpxCopy.Collections

/// <summary>
/// A sequence that is constrained to have at least one element.
/// </summary>
type NonEmpty<'a, 'b when 'a :> 'b seq> = 
  // private
  | NonEmpty of 'a

  interface IEnumerable<'b> with
    member this.GetEnumerator () = 
      let (NonEmpty xs) = this
      in (xs :> _ seq).GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (NonEmpty xs) = this
      in upcast ((xs :> _ seq).GetEnumerator ())

/// <summary>
/// A lazily evaluated sequence that is constrained to be finite and to have at least one element.
/// An alias for <c>NonEmpty<'a fseq, 'a></c>
/// </summary>
type NonEmptySeq<'a> = NonEmpty<'a fseq, 'a>
  
[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:#seq<'a>) = 
    if Seq.isEmpty xs
    then Empty
    else NotEmpty (NonEmpty xs)

  let (|NonEmpty|) (NonEmpty xs) = xs
  let (|NonEmptySeq|) (NonEmpty xs) : NonEmptySeq<_> = xs  


module FSeq = 
  /// <summary>
  /// Functions for manipulating NonEmpty FSeqs 
  /// </summary>
  module NonEmpty =
    /// <summary>
    /// Creates a new NonEmptySeq with the provided head and tail.  
    /// The tail is constrained to be finite.  If the tail is infinite,
    /// use Seq.NonEmpty.create instead
    /// </summary>
    let create head tail : NonEmptySeq<_> = NonEmpty (FiniteSeq.cons head tail)
   
    /// <summary>
    /// Returns the first element of the sequence.
    /// </summary>
    let head (NonEmptySeq xs) = Seq.head xs
    
    /// <summary>
    /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.
    /// </summary
    let min (NonEmptySeq xs) = Seq.min xs

    /// <summary>
    /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c>.
    /// </summary
    let max (NonEmptySeq xs) = Seq.max xs

    /// <summary>
    /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c> on the function result.
    /// </summary>
    let minBy projection (NonEmptySeq xs) = Seq.minBy projection xs

    /// <summary>
    /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c> on the function result.
    /// </summary>
    let maxBy projection (NonEmptySeq xs) = Seq.maxBy projection xs

    /// <summary>
    /// Returns the sequence after removing the first element.
    /// </summary>
    let tail (NonEmptySeq xs) = FiniteSeq.ofSeq (Seq.tail xs)
    
    /// <summary>
    /// Returns the tuple of the sequence's head and tail
    /// </summary>
    let uncons xs = (head xs, tail xs)
    
    /// <summary>
    /// Applies a function to each element of the sequence, threading an accumulator argument
    /// through the computation. Begin by applying the function to the first two elements.
    /// Then feed this result into the function along with the third element and so on.
    /// Return the final result.
    /// </summary>
    let reduce f (NonEmptySeq xs) = Seq.reduce f xs

    /// <summary>
    /// Returns the length of the sequence.
    /// </summary>
    let length (NonEmptySeq xs) = FiniteSeq.length xs
    
    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f (... (f s i0)...) iN</c>
    /// </summary>
    let fold f initialState (NonEmptySeq xs) = FiniteSeq.fold f initialState xs

    /// <summary>
    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The given function will be applied
    /// as elements are demanded using the MoveNext method on enumerators retrieved from the
    /// object.
    /// </summary>
    let map f (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.map f xs)
    
    /// <summary>
    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The integer index passed to the
    /// function indicates the index (from 0) of element being transformed.
    /// </summary>
    let mapi f (NonEmptySeq xs) =
      NonEmptySeq (FiniteSeq.mapi f xs)
    
    /// <summary>
    /// O(1). Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
    /// </summary>
    let map2 f (NonEmptySeq xs) (NonEmptySeq ys) =
      NonEmptySeq (FSeq.map2 f xs ys)

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true". This is a synonym for Seq.where.
    /// </summary>
    let filter f (NonEmptySeq xs) = FiniteSeq.filter f xs

    /// <summary>
    /// Wraps the two given enumerations as a single concatenated enumeration.
    /// </summary>
    let append xs (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.append xs ys)

    /// <summary>
    /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
    /// </summary>
    let concat (NonEmptySeq xs : NonEmptySeq<NonEmptySeq<'a>>) = 
      NonEmptySeq (xs |> FiniteSeq.map (fun (NonEmptySeq x) -> x) |> FiniteSeq.concat)

    /// <summary>
    /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
    /// the input list.
    /// </summary>
    let drop n (NonEmptySeq xs) = FiniteSeq.drop n xs

    type SeqIsEmpty = SeqIsEmpty of string
    /// <summary>
    /// Asserts that <c>xs</c> is not empty, creating a NonEmptySeq.
    /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
    /// </summary>
    let ofSeqSafe xs = 
      match xs with
      | Empty -> Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
      | NotEmpty ys -> Ok ys

    /// <summary>
    /// Asserts that <c>xs</c> is not empty, creating a NonEmptySeq.
    /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
    /// </summary>
    let inline ofSeq' xs = ofSeqSafe xs

    /// <summary>
    /// Returns a sequence of each element in the input sequence and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.
    /// </summary>
    let pairwise (NonEmptySeq xs) = FiniteSeq.pairwise xs

    /// <summary>
    /// Returns a new sequence with the elements in reverse order.
    /// </summary>
    let rev (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.rev xs)

    /// <summary>
    /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
    /// </summary>
    let scan f initialState (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.scan f initialState xs)

    /// <summary>
    /// Builds an array from the given collection.
    /// </summary>
    let toArray (NonEmptySeq xs) = FiniteSeq.toArray xs

    /// <summary>
    /// Builds a List from the given collection.
    /// </summary>
    let toList (NonEmptySeq xs) = FiniteSeq.toList xs

    /// <summary>
    /// Views the given NonEmptySeq as a sequence.
    /// </summary>
    let toSeq (NonEmptySeq xs) : _ seq = upcast xs 

    /// <summary>
    /// Returns the first element for which the given function returns True.
    /// Return None if no such element exists.
    /// </summary>
    let tryFind predicate (NonEmptySeq xs) = FiniteSeq.tryFind predicate xs

    /// <summary>
    /// O(n), where n is count. Return option the list which skips the first 'n' elements of
    /// the input list.
    /// </summary>
    let trySkip n (NonEmptySeq xs) = FiniteSeq.trySkip n xs

    /// <summary>
    /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
    /// the input list.
    /// </summary>
    let tryTake n (NonEmptySeq xs) = FiniteSeq.tryTake n xs

    /// <summary>
    /// Combines the two sequences into a list of pairs. 
    /// Returns None if the sequences are different lengths
    /// </summary>
    let tryZip (NonEmptySeq xs) (NonEmptySeq ys) = Option.map NonEmptySeq (FiniteSeq.tryZip xs ys)

    /// <summary>
    /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequence are ignored.
    /// </summary>
    let zip (NonEmptySeq xs) (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.zip xs ys)

/// <summary>
/// Functions for working with the NonEmptySeq type.  This exists for compatibility reasons, but it is
/// recommended that you use the equivalent module FSeq.NonEmpty, since this follows the same pattern as
/// other NonEmpty modules, like Seq.NonEmpty and List.NonEmpty
/// </summary>
module NonEmptySeq =
  /// <summary>
  /// Creates a new NonEmptySeq with the provided head and tail.  
  /// The tail is constrained to be finite.  If the tail is infinite,
  /// use Seq.NonEmpty.create instead
  /// </summary>
  let create head tail : NonEmptySeq<_> = FSeq.NonEmpty.create head tail
 
  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let head xs = FSeq.NonEmpty.head xs
  
  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.
  /// </summary
  let min (NonEmptySeq xs) = Seq.min xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c>.
  /// </summary
  let max (NonEmptySeq xs) = Seq.max xs

  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c> on the function result.
  /// </summary>
  let minBy projection (NonEmptySeq xs) = Seq.minBy projection xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c> on the function result.
  /// </summary>
  let maxBy projection (NonEmptySeq xs) = Seq.maxBy projection xs

  /// <summary>
  /// Returns the sequence after removing the first element.
  /// </summary>
  let tail (NonEmptySeq xs) = FiniteSeq.ofSeq (Seq.tail xs)
  
  /// <summary>
  /// Returns the tuple of the sequence's head and tail
  /// </summary>
  let uncons xs = (head xs, tail xs)
  
  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.
  /// </summary>
  let reduce f (NonEmptySeq xs) = Seq.reduce f xs

  /// <summary>
  /// Returns the length of the sequence.
  /// </summary>
  let length (NonEmptySeq xs) = FiniteSeq.length xs
  
  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let fold f initialState (NonEmptySeq xs) = FiniteSeq.fold f initialState xs

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let map f (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.map f xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (NonEmptySeq xs) =
    NonEmptySeq (FiniteSeq.mapi f xs)
  
  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2 f (NonEmptySeq xs) (NonEmptySeq ys) =
    NonEmptySeq (FSeq.map2 f xs ys)

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let filter f (NonEmptySeq xs) = FiniteSeq.filter f xs

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let append xs (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.append xs ys)

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let concat (NonEmptySeq xs : NonEmptySeq<NonEmptySeq<'a>>) = 
    NonEmptySeq (xs |> FiniteSeq.map (fun (NonEmptySeq x) -> x) |> FiniteSeq.concat)

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  /// </summary>
  let drop n (NonEmptySeq xs) = FiniteSeq.drop n xs

  type SeqIsEmpty = SeqIsEmpty of string
  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofSeqSafe xs = 
    match xs with
    | Empty -> Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    | NotEmpty ys -> Ok ys

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofSeq' xs = ofSeqSafe xs

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (NonEmptySeq xs) = FiniteSeq.pairwise xs

  /// <summary>
  /// Returns a new sequence with the elements in reverse order.
  /// </summary>
  let rev (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.rev xs)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (NonEmptySeq xs) = NonEmptySeq (FiniteSeq.scan f initialState xs)

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let toArray (NonEmptySeq xs) = FiniteSeq.toArray xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let toList (NonEmptySeq xs) = FiniteSeq.toList xs

  /// <summary>
  /// Views the given NonEmptySeq as a sequence.
  /// </summary>
  let toSeq (NonEmptySeq xs) : _ seq = upcast xs 

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let tryFind predicate (NonEmptySeq xs) = FiniteSeq.tryFind predicate xs

  /// <summary>
  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let trySkip n (NonEmptySeq xs) = FiniteSeq.trySkip n xs

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let tryTake n (NonEmptySeq xs) = FiniteSeq.tryTake n xs

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths
  /// </summary>
  let tryZip (NonEmptySeq xs) (NonEmptySeq ys) = Option.map NonEmptySeq (FiniteSeq.tryZip xs ys)

  /// <summary>
  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let zip (NonEmptySeq xs) (NonEmptySeq ys) = NonEmptySeq (FiniteSeq.zip xs ys)

