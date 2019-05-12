namespace SafetyFirst

open System
open System.Collections
open System.Collections.Generic

open ResultDotNet.FSharp

open SafetyFirst.FSharpxCopy.Collections
open SafetyFirst.ErrorTypes  
open SafetyFirst.Numbers

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
/// An alias for <c>NonEmpty<'a fseq, 'a></c>
/// </summary>
type NonEmptyFSeq<'a> = NonEmpty<'a fseq, 'a>

[<AutoOpen>]
module FSeqBuilder = 
  /// <summary>
  /// A lazy sequence constrained to be finite in length.  There is no possible runtime check
  /// for whether or not a seq is infinite, so this is more of an assertion of the programmer
  /// that this particular seq is finite.
  /// </summary>
  let fseq (xs:_ seq) = FiniteSeq xs
  let (|FiniteSeq|) (xs:_ fseq) = xs.Values
  let (|FSeq|) (xs:_ fseq) = xs.Values


module FiniteSeq =

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let inline append (FSeq xs) (FSeq ys) = fseq (LazyList.append xs ys)

  /// <summary>
  /// Applies the given function to each element of the list. Return the list comprised of 
  /// the results "x" for each element where the function returns Some(x).
  /// The returned sequence may be passed between threads safely. 
  /// However, individual IEnumerator values generated from the returned sequence should 
  /// not be accessed concurrently.
  /// </summary>
  let choose chooser source = fseq (Seq.choose chooser source)

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let inline concat (FSeq xs : FiniteSeq<FiniteSeq<'a>>) = fseq (xs |> LazyList.map (|FSeq|) |> LazyList.concat)

  /// <summary>
  /// Applies the given function to each element of the sequence and concatenates all the results.
  /// Returned sequence is lazy, effects are delayed until it is enumerated.
  /// </summary>
  let inline collect (f : 'a -> FiniteSeq<'b>) (FSeq xs : FiniteSeq<'a>) = 
    fseq (xs |> LazyList.map f |> LazyList.map (|FSeq|) |> LazyList.concat)

  /// <summary>
  /// O(1). Return a new list which contains the given item followed by the
  /// given list.
  /// </summary>
  let cons head (FSeq xs) = fseq (LazyList.cons head xs)

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  /// </summary>
  let inline drop n (FSeq xs) = fseq (LazyList.drop n xs)

  /// <summary>
  /// O(n), where n is count. Return the seq which will remove at most 'n' elements of
  /// the input seq.
  /// This function will return the input seq unaltered for negative values of 'n'.
  /// </summary>
  let dropLenient n xs = 
    match n with
    | Natural i -> drop i xs
    | neg -> xs  

  /// <summary>
  /// O(1). Evaluates to the sequence that contains no items
  /// </summary>
  let empty<'a> = fseq (LazyList.empty<'a>)

  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let inline fold f initialState (FSeq xs) = LazyList.fold f initialState xs

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let inline filter f (FSeq xs) = fseq (LazyList.filter f xs)

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let tryFind predicate (FSeq xs) = LazyList.tryFind predicate xs

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Returns a NoMatchingElement Error if no such element is found.
  /// </summary>
  let findSafe predicate xs = 
    tryFind predicate xs |> Result.ofOption findErr
  
  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Returns a NoMatchingElement Error if no such element is found.
  /// </summary>
  let inline find' predicate xs = findSafe predicate xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let tryHead (FSeq xs) = LazyList.head xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let headSafe xs = tryHead xs |> Result.ofOption headErr

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let inline head' xs = headSafe xs

  /// <summary>
  /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
  /// </summary>
  let indexed xs = fseq (Seq.indexed xs)

  /// <summary>
  /// Returns true if the sequence contains no elements, false otherwise.
  /// </summary>
  let inline isEmpty (FSeq xs) = LazyList.isEmpty xs
  
  /// <summary>
  /// Returns the length of the sequence
  /// </summary>
  let inline length (xs : _ fseq) = xs.Length

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let inline map f (FSeq xs) = fseq (LazyList.map f xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (FSeq xs) =
    fseq (LazyList.map2 f (LazyList.ofSeq (Seq.initInfinite id)) xs)

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.
  /// </summary>
  let inline map2 f (FSeq xs) (FSeq ys) = 
    fseq (LazyList.map2 f xs ys)

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let map2Safe f xs ys =
    if length xs <> length ys 
    then Error (map2Err (length xs) (length ys)) 
    else Ok (map2 f xs ys)

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline map2' f xs ys = map2Safe f xs ys

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns None if the sequences are different lengths.
  /// </summary>
  let tryMap2 f xs ys = map2' f xs ys |> Result.toOption

  /// <summary>
  /// Views the given array as a finite sequence.
  /// </summary>
  let inline ofArray xs = fseq (LazyList.ofArray xs)
  
  /// <summary>
  /// Views the given seq as a finite sequence.  There is no runtime validation
  /// that the seq is actually finite, so this is a programmer assertion that the
  /// seq will be finite.
  /// </summary>
  let inline ofSeq xs = fseq (LazyList.ofSeq xs) 

  /// <summary>
  /// Views the given list as a finite sequence.  
  /// </summary>
  let inline ofList xs = fseq (LazyList.ofList xs)

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (FSeq xs) = fseq (Seq.pairwise xs)

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty
  /// </summary>
  let reduceSafe f (FSeq xs) = 
    match LazyList.uncons xs with
    | Some (head, tail) -> Ok <| LazyList.fold f head tail
    | None -> Error reduceErr

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty
  /// </summary>
  let inline reduce' f xs = reduceSafe f xs   

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns None if the sequence is empty
  /// </summary>
  let tryReduce f xs = reduce' f xs |> Result.toOption
  
  /// <summary>
  /// Returns a new sequence with the elements in reverse order.
  /// </summary>
  let inline rev (FSeq xs) = fseq (LazyList.rev xs)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let inline scan f initialState (FSeq xs) = fseq (LazyList.scan f initialState xs)

  /// <summary>
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let trySkip n (FSeq xs) = Option.map fseq (LazyList.skip n xs)

  /// <summary>
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let skipSafe n xs = 
    trySkip n xs 
    |> Result.ofOptionWith (fun () -> skipErr n (length xs))
    
  /// <summary>
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let inline skip' n xs = skipSafe n xs  

  /// <summary>
  /// Returns a sequence that skips at least N elements of the underlying sequence and then yields the
  /// remaining elements of the sequence.
  /// Returns an empty sequence if <c>count</c> exceeds the length of <c>xs</c> 
  /// </summary>
  let skipLenient count xs = 
    skip' count xs 
    |> Result.defaultValue (fseq [])
  
  /// <summary>
  /// Returns a sequence that, when iterated, skips elements of the underlying sequence while the
  /// given predicate returns True, and then yields the remaining elements of the sequence.
  /// </summary>
  let skipWhile predicate (FSeq xs) = 
    fseq (Seq.skipWhile predicate xs)

  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let tryTail (FSeq xs) = Option.map fseq (LazyList.tail xs)
  
  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let tailSafe xs = tryTail xs |> Result.ofOption tailErr

  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let inline tail' xs = tailSafe xs

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let tryTake n (FSeq xs) = Option.map fseq (LazyList.take n xs)

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let takeSafe n xs = 
    tryTake n xs 
    |> Result.ofOptionWith (fun () -> takeErr n (length xs))

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let inline take' n xs = takeSafe n xs 

  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.
  /// </summary>
  let takeWhile predicate (FSeq xs) =
    fseq (Seq.takeWhile predicate xs)

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let inline toArray (FSeq xs) = LazyList.toArray xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let inline toList (FSeq xs) = LazyList.toList xs
  
  /// <summary>
  /// Views the given FiniteSeq as a sequence.
  /// </summary>
  let inline toSeq (FSeq xs) : _ seq = upcast xs

  /// <summary>
  /// Returns a sequence that when enumerated returns at most N elements.
  /// </summary>
  let truncate n (FSeq xs) = fseq (LazyList.ofSeq (Seq.truncate n xs))  

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let unconsSafe (FSeq xs) = 
    match LazyList.uncons xs with
    | Some (head, tail) -> Ok (head, fseq tail)
    | None -> Error unconsErr

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let inline uncons' xs = unconsSafe xs

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let tryUncons xs = uncons' xs |> Result.toOption 

  /// <summary>
  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let inline zip (FSeq xs) (FSeq ys) = fseq (LazyList.zip xs ys)

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let zipSafe xs ys =
    if length xs <> length ys 
    then Error (zipErr (length xs) (length ys)) 
    else Ok (zip xs ys)

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline zip' xs ys = zipSafe xs ys  

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths.
  /// </summary>
  let tryZip xs ys = zipSafe xs ys |> Result.toOption
  
module FSeq =
  /// <summary>
  /// Returns the length of the sequence
  /// </summary>
  let inline length xs = FiniteSeq.length xs

  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let inline fold f initialState xs = FiniteSeq.fold f initialState xs

  /// <summary>
  /// Returns true if the sequence contains no elements, false otherwise.
  /// </summary>
  let inline isEmpty xs = FiniteSeq.isEmpty xs

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty.
  /// </summary>
  let inline reduceSafe f xs = FiniteSeq.reduceSafe f xs

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty.
  /// </summary>
  let inline reduce' f xs = FiniteSeq.reduce' f xs

  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns None if the sequence is empty
  /// </summary>
  let inline tryReduce f xs = FiniteSeq.tryReduce f xs

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let inline map f xs = FiniteSeq.map f xs

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let inline mapi f xs = FiniteSeq.mapi f xs

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.
  /// </summary>
  let inline map2 f xs ys = FiniteSeq.map2 f xs ys

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let inline filter f xs = FiniteSeq.filter f xs

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let inline append xs ys = FiniteSeq.append xs ys

  /// <summary>
  /// Applies the given function to each element of the list. Return the list comprised of 
  /// the results "x" for each element where the function returns Some(x).
  /// The returned sequence may be passed between threads safely. 
  /// However, individual IEnumerator values generated from the returned sequence should 
  /// not be accessed concurrently.
  /// </summary>
  let inline choose chooser source = FiniteSeq.choose chooser source

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let inline concat xs = FiniteSeq.concat xs

  /// <summary>
  /// Applies the given function to each element of the sequence and concatenates all the results.
  /// Returned sequence is lazy, effects are delayed until it is enumerated.
  /// </summary>
  let inline collect xs = FiniteSeq.collect xs

  /// <summary>
  /// O(1). Return a new list which contains the given item followed by the
  /// given list.
  /// </summary>
  let inline cons head xs = FiniteSeq.cons head xs

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  /// </summary>
  let inline drop n xs = FiniteSeq.drop n xs

  /// <summary>
  /// O(n), where n is count. Return the seq which will remove at most 'n' elements of
  /// the input seq.
  /// This function will return the input seq unaltered for negative values of 'n'.
  /// </summary>
  let dropLenient n xs = FiniteSeq.dropLenient n xs

  /// <summary>
  /// O(1). Evaluates to the sequence that contains no items
  /// </summary>
  let empty<'a when 'a : comparison> = FiniteSeq.empty<'a>

  /// <summary>
  /// Views the given array as a finite sequence.
  /// </summary>
  let inline ofArray xs = FiniteSeq.ofArray xs

  /// <summary>
  /// Views the given seq as a finite sequence.  There is no runtime validation
  /// that the seq is actually finite, so this is a programmer assertion that the
  /// seq will be finite.
  /// </summary>
  let inline ofSeq xs = FiniteSeq.ofSeq xs 

  /// <summary>
  /// Views the given list as a finite sequence.  
  /// </summary>
  let inline ofList xs = FiniteSeq.ofList xs

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let inline pairwise xs = FiniteSeq.pairwise xs

  /// <summary>
  /// Returns a new sequence with the elements in reverse order.
  /// </summary>
  let inline rev xs = FiniteSeq.rev xs

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let inline scan f initialState xs = FiniteSeq.scan f initialState xs

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let inline toArray xs = FiniteSeq.toArray xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let inline toList xs = FiniteSeq.toList xs

  /// <summary>
  /// Views the given FiniteSeq as a sequence.
  /// </summary>
  let inline toSeq xs = FiniteSeq.toSeq

  /// <summary>
  /// Returns a sequence that when enumerated returns at most N elements.
  /// </summary>
  let inline truncate n xs = FiniteSeq.truncate n xs

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let inline tryFind predicate xs = FiniteSeq.tryFind predicate xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let inline tryHead xs = FiniteSeq.tryHead xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let inline headSafe xs = FiniteSeq.headSafe xs

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let inline head' xs = FiniteSeq.head' xs

  /// <summary>
  /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
  /// </summary>
  let inline indexed xs = FiniteSeq.indexed xs

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns None if the sequences are different lengths
  /// </summary>
  let inline tryMap2 f xs ys = FiniteSeq.tryMap2 f xs ys 

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline map2Safe f xs ys = FiniteSeq.map2Safe f xs ys

  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline map2' f xs ys = FiniteSeq.map2' f xs ys

  /// <summary>
  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let inline trySkip n xs = FiniteSeq.trySkip n xs

  /// <summary>
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let inline skipSafe n xs = FiniteSeq.skipSafe n xs

  /// <summary>
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let inline skip' n xs = FiniteSeq.skip' n xs

  /// <summary>
  /// Returns a sequence that, when iterated, skips elements of the underlying sequence while the
  /// given predicate returns True, and then yields the remaining elements of the sequence.
  /// </summary>
  let inline skipWhile predicate xs = FiniteSeq.skipWhile predicate xs

  /// <summary>
  /// Returns a sequence that, when iterated, yields elements of the underlying sequence while the
  /// given predicate returns True, and then returns no further elements.
  /// </summary>
  let inline takeWhile predicate xs = FiniteSeq.takeWhile predicate xs

  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let inline tryTail xs = FiniteSeq.tryTail xs

  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let inline tailSafe xs = FiniteSeq.tailSafe xs

  /// <summary>
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  /// </summary>
  let inline tail' xs = FiniteSeq.tail' xs

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let inline tryTake n xs = FiniteSeq.tryTake n xs
    
  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let inline takeSafe n xs = FiniteSeq.takeSafe n xs
    
  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let inline take' n xs = FiniteSeq.take' n xs

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let inline tryUncons xs = FiniteSeq.tryUncons xs

  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let inline unconsSafe xs = FiniteSeq.unconsSafe xs
  
  /// <summary>
  /// O(1). Returns tuple of head element and tail of the list.
  /// </summary>
  let inline uncons' xs = FiniteSeq.uncons' xs

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths
  /// </summary>
  let inline tryZip xs ys = FiniteSeq.tryZip xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline zipSafe xs ys = FiniteSeq.zipSafe xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  /// </summary>
  let inline zip' xs ys = FiniteSeq.zip' xs ys

  /// <summary>
  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let inline zip xs ys = FiniteSeq.zip xs ys

  /// <summary>
  /// Functions for manipulating NonEmpty FSeqs 
  /// </summary>
  module NonEmpty =
    let (|NonEmptyFSeq|) (NonEmpty xs : NonEmptyFSeq<_>) = xs

    /// <summary>
    /// Creates a new NonEmpty FSeq with the provided head and tail.  
    /// The tail is constrained to be finite.  If the tail is infinite,
    /// use Seq.NonEmpty.create instead
    /// </summary>
    let create head tail : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.cons head tail)

    /// <summary>
    /// Returns a sequence that yields one item only.
    /// </summary>
    let singleton x : NonEmptyFSeq<_> = NonEmpty (fseq [x])

    /// <summary>
    /// Returns the average of the elements in the sequence.
    /// The elements are averaged using the <c>+</c> operator, 
    /// <c>DivideByInt</c> method and <c>Zero</c> property associated with the element type.
    /// </summary>
    let inline average (NonEmptyFSeq xs) = Seq.average xs

    /// <summary>
    /// Returns the first element of the sequence.
    /// </summary>
    let head (NonEmptyFSeq xs) = Seq.head xs
    
    /// <summary>
    /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.
    /// </summary
    let min (NonEmptyFSeq xs) = Seq.min xs

    /// <summary>
    /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c>.
    /// </summary
    let max (NonEmptyFSeq xs) = Seq.max xs

    /// <summary>
    /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c> on the function result.
    /// </summary>
    let minBy projection (NonEmptyFSeq xs) = Seq.minBy projection xs

    /// <summary>
    /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c> on the function result.
    /// </summary>
    let maxBy projection (NonEmptyFSeq xs) = Seq.maxBy projection xs

    /// <summary>
    /// Returns the sequence after removing the first element.
    /// </summary>
    let tail (NonEmptyFSeq xs) = FiniteSeq.ofSeq (Seq.tail xs)
    
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
    let reduce f (NonEmptyFSeq xs) = Seq.reduce f xs

    /// <summary>
    /// Returns the length of the sequence.
    /// </summary>
    let length (NonEmptyFSeq xs) = FiniteSeq.length xs
    
    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f (... (f s i0)...) iN</c>
    /// </summary>
    let fold f initialState (NonEmptyFSeq xs) = FiniteSeq.fold f initialState xs

    /// <summary>
    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The given function will be applied
    /// as elements are demanded using the MoveNext method on enumerators retrieved from the
    /// object.
    /// </summary>
    let map f (NonEmptyFSeq xs) : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.map f xs)
    
    /// <summary>
    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The integer index passed to the
    /// function indicates the index (from 0) of element being transformed.
    /// </summary>
    let mapi f (NonEmptyFSeq xs) : NonEmptyFSeq<_> =
      NonEmpty (FiniteSeq.mapi f xs)
    
    /// <summary>
    /// O(1). Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
    /// </summary>
    let map2 f (NonEmptyFSeq xs) (NonEmptyFSeq ys) : NonEmptyFSeq<_> =
      NonEmpty (FiniteSeq.map2 f xs ys)

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true". This is a synonym for Seq.where.
    /// </summary>
    let filter f (NonEmptyFSeq xs) = FiniteSeq.filter f xs

    /// <summary>
    /// Wraps the two given enumerations as a single concatenated enumeration.
    /// </summary>
    let append xs (NonEmptyFSeq ys) : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.append xs ys)

    /// <summary>
    /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
    /// </summary>
    let concat (NonEmptyFSeq xs : NonEmptyFSeq<NonEmptyFSeq<'a>>) : NonEmptyFSeq<_> = 
      NonEmpty (xs |> FiniteSeq.map (fun (NonEmptyFSeq x) -> x) |> FiniteSeq.concat)

    /// <summary>
    /// Applies the given function to each element of the sequence and concatenates all the results.
    /// Returned sequence is lazy, effects are delayed until it is enumerated.
    /// </summary>
    let collect (f : 'a -> NonEmptyFSeq<'b>) (NonEmptyFSeq xs : NonEmptyFSeq<'a>) : NonEmptyFSeq<'b> = 
      let g = f >> (|NonEmpty|)
      NonEmpty (collect g xs)

    /// <summary>
    /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
    /// the input list.
    /// </summary>
    let drop n (NonEmptyFSeq xs) = FiniteSeq.drop n xs

    /// <summary>
    /// O(n), where n is count. Return the seq which will remove at most 'n' elements of
    /// the input seq.
    /// This function will return the input seq unaltered for negative values of 'n'.
    /// </summary>
    let dropLenient n (NonEmptyFSeq xs) = FiniteSeq.dropLenient n xs

    /// <summary>
    /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
    /// </summary>
    let indexed (NonEmptyFSeq xs) : NonEmptyFSeq<_> = NonEmpty (indexed xs)

    /// <summary>
    /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
    /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
    /// </summary>
    let ofFSeqSafe (xs:_ fseq) : Result<NonEmptyFSeq<_>,_> = 
      match xs with
      | Empty -> Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
      | NotEmpty ys -> Ok <| ys

    /// <summary>
    /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
    /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
    /// </summary>
    let inline ofFSeq' xs = ofFSeqSafe xs

    /// <summary>
    /// Returns a sequence of each element in the input sequence and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.
    /// </summary>
    let pairwise (NonEmptyFSeq xs) = FiniteSeq.pairwise xs

    /// <summary>
    /// Returns a new sequence with the elements in reverse order.
    /// </summary>
    let rev (NonEmptyFSeq xs) : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.rev xs)

    /// <summary>
    /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
    /// </summary>
    let scan f initialState (NonEmptyFSeq xs) : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.scan f initialState xs)

    /// <summary>
    /// Builds an array from the given collection.
    /// </summary>
    let toArray (NonEmptyFSeq xs) = FiniteSeq.toArray xs

    /// <summary>
    /// Builds a NonEmpty array from the given collection.
    /// </summary>
    let toNonEmptyArray xs : NonEmptyArray<_> = NonEmpty <| toArray xs

    /// <summary>
    /// Builds a List from the given collection.
    /// </summary>
    let toList (NonEmptyFSeq xs) = FiniteSeq.toList xs

    /// <summary>
    /// Builds a NonEmpty List from the given collection.
    /// </summary>
    let toNonEmptyList xs : NonEmptyList<_> = NonEmpty <| toList xs

    /// <summary>
    /// Views the given NonEmpty FSeq as a sequence.
    /// </summary>
    let toSeq (NonEmptyFSeq xs) : _ seq = upcast xs 

    /// <summary>
    /// Builds a NonEmpty FSeq from the given NonEmpty Seq
    /// </summary>
    let ofNonEmptySeq (NonEmpty xs) : NonEmptyFSeq<_> = NonEmpty (fseq xs)

    /// <summary>
    /// Views the given NonEmpty FSeq as a NonEmpty Seq
    /// </summary>
    let toNonEmptySeq xs : NonEmptySeq<_> = NonEmpty <| toSeq xs

    /// <summary>
    /// Views the given NonEmpty FSeq as an FSeq.
    /// </summary>
    let toFSeq (NonEmptyFSeq xs) = xs

    /// <summary>
    /// Returns a sequence that when enumerated returns at most n elements.
    /// </summary>
    let truncate (PositiveInt n) (NonEmpty xs) : NonEmptyFSeq<_> = 
      NonEmpty (truncate n xs)

    /// <summary>
    /// Returns the first element for which the given function returns True.
    /// Return None if no such element exists.
    /// </summary>
    let tryFind predicate (NonEmptyFSeq xs) = FiniteSeq.tryFind predicate xs

    /// <summary>
    /// O(n), where n is count. Return option the list which skips the first 'n' elements of
    /// the input list.
    /// </summary>
    let trySkip n (NonEmptyFSeq xs) = FiniteSeq.trySkip n xs

    /// <summary>
    /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
    /// the input list.
    /// </summary>
    let tryTake n (NonEmptyFSeq xs) = FiniteSeq.tryTake n xs

    /// <summary>
    /// Combines the two sequences into a list of pairs. 
    /// Returns None if the sequences are different lengths
    /// </summary>
    let tryZip (NonEmptyFSeq xs) (NonEmptyFSeq ys) : Option<NonEmptyFSeq<_>> = Option.map NonEmpty (FiniteSeq.tryZip xs ys)

    /// <summary>
    /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequence are ignored.
    /// </summary>
    let zip (NonEmptyFSeq xs) (NonEmptyFSeq ys) : NonEmptyFSeq<_> = NonEmpty (FiniteSeq.zip xs ys)

    /// <summary>
    /// Splits a sequence at every occurrence of an element satisfying <c>splitAfter</c>.
    /// The split occurs immediately after each element that satisfies <c>splitAfter</c>,
    /// and the element satisfying <c>splitAfter</c> will be included as the last element of 
    /// the sequence preceeding the split.
    /// For example:
    /// <code>
    /// split ((=) 100) (FSeq.NonEmpty.create 1[2;3;100;100;4;100;5;6])
    ///   //returns ([[1;2;3;100];[100];[4;100];[5;6]])
    /// </code>
    /// </summary>
    // this implementation is faster than the version in Seq.NonEmpty, but is unsafe for infinite sequences
    // so this should be the default used for any finite sequence (inculding lists and arrays) 
    let split splitAfter xs = 
      let addToEnd xs x = append xs (singleton x)
      let (++) = addToEnd

      let rec split' (input:'a fseq) startNewGroup (currentGroup:NonEmptyFSeq<'a>) (completedGroups:fseq<NonEmptyFSeq<'a>>) =
        match input with
        | NotEmpty input ->
          let (head, tail) = uncons input

          let newCurrentGroup, newCompletedGroups = 
            if not startNewGroup
            then (fseq currentGroup ++ head, completedGroups)
            else (singleton head, fseq (completedGroups ++ currentGroup))

          split' tail (splitAfter head) newCurrentGroup newCompletedGroups

        | Empty ->
          completedGroups ++ currentGroup

      let (head, tail) = uncons xs
      split' tail (splitAfter head) (singleton head) (fseq [])

    /// <summary>
    /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
    /// For example:
    /// <code>
    /// splitPairwise (=) (Seq.NonEmpty.create 0[1;1;2;3;4;4;4;5])
    ///   //returns [[0;1];[1;2;3;4];[4];[4;5]]
    /// </code>
    /// </summary>
    // this implementation is faster than the version in Seq.NonEmpty, but is unsafe for infinite sequences
    // so this should be the default used for any finite sequence (inculding lists and arrays) 
    let splitPairwise splitBetween xs : NonEmptyFSeq<NonEmptyFSeq<_>> =
      let (++) = append

      let rec split' (input:'a fseq) (previousElement:'a) (currentGroup:NonEmptyFSeq<'a>) (completedGroups:fseq<NonEmptyFSeq<'a>>) =
        match input with
        | Empty -> completedGroups ++ singleton currentGroup
        | NotEmpty input -> 
          let (head, tail) = uncons input
          let newInput = tail
          let newPrev = head
          if splitBetween previousElement head
          then 
            let newGroup = (singleton head)
            let newCompletedGroups = (completedGroups ++ singleton currentGroup) 
            split' newInput newPrev newGroup (fseq newCompletedGroups)
          else 
            let expandedGroup = (fseq currentGroup) ++ singleton head
            split' newInput newPrev expandedGroup completedGroups

      let (head, tail) = uncons xs
      split' tail head (singleton head) (fseq [])

open System.Runtime.CompilerServices

[<Extension>]
module EnumerableExtensions = 
  /// <summary>
  /// Creates a FiniteSeq, or a lazy sequence constrained to be finite in length.  There is no possible runtime check
  /// for whether or not an IEnumerable is infinite, so this is more of an assertion of the programmer
  /// that this particular IEnumerable is finite.
  /// </summary>
  [<Extension>]
  let Finite (xs) = fseq xs

