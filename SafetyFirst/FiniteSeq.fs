namespace SafetyFirst

open System
open System.Collections
open System.Collections.Generic
open FSharpx.Collections

open ResultDotNet.FSharp

open SafetyFirst.ErrorTypes

/// A lazy sequence constrained to be finite in length.
type FiniteSeq<'a when 'a : comparison> (xs : LazyList<'a>) = 
  //this implementation is modeled off of how array hashes are computed, here: 
  //https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/FSharp.Core/prim-types.fs
  //starting at line 1680
  let hashCode = 
    lazy (
      let inline hashCombine nr x y = (x <<< 1) + y + 631 * nr

      let first18 = Seq.truncate 18 (Seq.indexed xs)
      first18 |> Seq.fold (fun acc (i, x) -> hashCombine i acc (hash x)) 0 
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
            Seq.map2 compare xs ys 
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
    match x with
    | :? FiniteSeq<'a> as xs ->
      let lengthCompare = 
        if this.HasCalculatedLength && xs.HasCalculatedLength then xs.Length = this.Length else true
      let hashCompare = 
        if this.HasCalculatedHash && xs.HasCalculatedHash then xs.GetHashCode() = this.GetHashCode() else true

      LanguagePrimitives.PhysicalEquality this xs 
      ||
      (
        lengthCompare 
        && 
        hashCompare
        &&
        compare this xs = 0
      )
    | _ -> false

  interface IEquatable<FiniteSeq<'a>> with
    member this.Equals x = this.Equals x

  override this.GetHashCode () = hashCode.Value

type FSeq<'a when 'a : comparison> = FiniteSeq<'a>

type fseq<'a when 'a : comparison> = FiniteSeq<'a>

[<AutoOpen>]
module FSeqBuilder = 
  /// A lazy sequence constrained to be finite in length.  There is no possible runtime check
  /// for whether or not a seq is infinite, so this is more of an assertion of the programmer
  /// that this particular seq is finite.
  let fseq (xs:_ seq) = FiniteSeq xs
  let (|FiniteSeq|) (xs:_ fseq) = xs.Values
  let (|FSeq|) (xs:_ fseq) = xs.Values

module FiniteSeq =
  /// Returns the length of the sequence
  let inline length (xs : _ fseq) = xs.Length

  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  let inline fold f initialState (FSeq xs) = LazyList.fold f initialState xs
  
  /// Returns true if the sequence contains no elements, false otherwise.
  let inline isEmpty (FSeq xs) = LazyList.isEmpty xs

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty
  let reduceSafe f (FSeq xs) = 
    match xs with
    | LazyList.Cons (head, tail) -> Ok <| LazyList.fold f head tail
    | LazyList.Nil -> Error reduceErr

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty
  let inline reduce' f xs = reduceSafe f xs   

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns None if the sequence is empty
  let tryReduce f xs = reduce' f xs |> Result.toOption

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  let inline map f (FSeq xs) = fseq (LazyList.map f xs)
  
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  let mapi f (FSeq xs) =
    fseq (LazyList.map2 f (LazyList.ofSeq (Seq.initInfinite id)) xs)

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.
  let inline map2 f (FSeq xs) (FSeq ys) = 
    fseq (LazyList.map2 f xs ys)

  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  let inline filter f (FSeq xs) = fseq (LazyList.filter f xs)

  /// Wraps the two given enumerations as a single concatenated enumeration.
  let inline append (FSeq xs) (FSeq ys) = fseq (LazyList.append xs ys)

  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  let inline concat (FSeq xs : FiniteSeq<FiniteSeq<'a>>) = fseq (xs |> LazyList.map (|FSeq|) |> LazyList.concat)

  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  let inline drop n (FSeq xs) = fseq (LazyList.drop n xs)

  /// Views the given array as a finite sequence.
  let inline ofArray xs = fseq (LazyList.ofArray xs)
  
  /// Views the given seq as a finite sequence.  There is no runtime validation
  /// that the seq is actually finite, so this is a programmer assertion that the
  /// seq will be finite.
  let inline ofSeq xs = fseq (LazyList.ofSeq xs) 

  /// Views the given list as a finite sequence.  
  let inline ofList xs = fseq (LazyList.ofList xs)
  
  /// Returns a new sequence with the elements in reverse order.
  let inline rev (FSeq xs) = fseq (LazyList.rev xs)

  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  let inline scan f initialState (FSeq xs) = fseq (LazyList.scan f initialState xs)

  /// Builds an array from the given collection.
  let inline toArray (FSeq xs) = LazyList.toArray xs

  /// Builds a List from the given collection.
  let inline toList (FSeq xs) = LazyList.toList xs
  
  /// Views the given FiniteSeq as a sequence.
  let inline toSeq (FSeq xs) : _ seq = upcast xs

  /// Returns a sequence that when enumerated returns at most N elements.
  let truncate n (FSeq xs) = fseq (LazyList.ofSeq (Seq.truncate n xs))  

  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  let tryFind predicate (FSeq xs) = LazyList.tryFind predicate xs

  /// Returns the first element for which the given function returns True.
  /// Returns a NoMatchingElement Error if no such element is found.
  let findSafe predicate xs = 
    tryFind predicate xs |> Result.ofOption findErr
  
  /// Returns the first element for which the given function returns True.
  /// Returns a NoMatchingElement Error if no such element is found.
  let inline find' predicate xs = findSafe predicate xs

  /// Returns the first element of the sequence.
  let tryHead (FSeq xs) = LazyList.tryHead xs

  /// Returns the first element of the sequence.
  let headSafe xs = tryHead xs |> Result.ofOption headErr

  /// Returns the first element of the sequence.
  let inline head' xs = headSafe xs

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let map2Safe f xs ys =
    if length xs <> length ys 
    then Error (map2Err (length xs) (length ys)) 
    else Ok (map2 f xs ys)

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline map2' f xs ys = map2Safe f xs ys

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns None if the sequences are different lengths.
  let tryMap2 f xs ys = map2' f xs ys |> Result.toOption

  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  let trySkip n (FSeq xs) = Option.map fseq (LazyList.trySkip n xs)

  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  let skipSafe n xs = 
    trySkip n xs 
    |> Result.ofOptionWith (fun () -> skipErr n (length xs))
    
  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  let inline skip' n xs = skipSafe n xs  

  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let tryTail (FSeq xs) = Option.map fseq (LazyList.tryTail xs)
  
  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let tailSafe xs = tryTail xs |> Result.ofOption tailErr

  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let inline tail' xs = tailSafe xs

  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let tryTake n (FSeq xs) = Option.map fseq (LazyList.tryTake n xs)

  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let takeSafe n xs = 
    tryTake n xs 
    |> Result.ofOptionWith (fun () -> takeErr n (length xs))

  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let inline take' n xs = takeSafe n xs 

  /// O(1). Returns tuple of head element and tail of the list.
  let unconsSafe (FSeq xs) = 
    match LazyList.tryUncons xs with
    | Some (head, tail) -> Ok (head, fseq tail)
    | None -> Error unconsErr

  /// O(1). Returns tuple of head element and tail of the list.
  let inline uncons' xs = unconsSafe xs

  /// O(1). Returns tuple of head element and tail of the list.
  let tryUncons xs = uncons' xs |> Result.toOption 

  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  let inline zip (FSeq xs) (FSeq ys) = fseq (LazyList.zip xs ys)

  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let zipSafe xs ys =
    if length xs <> length ys 
    then Error (zipErr (length xs) (length ys)) 
    else Ok (zip xs ys)

  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline zip' xs ys = zipSafe xs ys  

  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths.
  let tryZip xs ys = zipSafe xs ys |> Result.toOption
  
module FSeq =
  /// Returns the length of the sequence
  let inline length xs = FiniteSeq.length xs

  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  let inline fold f initialState xs = FiniteSeq.fold f initialState xs

  /// Returns true if the sequence contains no elements, false otherwise.
  let inline isEmpty xs = FiniteSeq.isEmpty xs

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty.
  let inline reduceSafe f xs = FiniteSeq.reduceSafe f xs

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns a SeqIsEmpty Error if the sequence is empty.
  let inline reduce' f xs = FiniteSeq.reduce' f xs

  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.  
  /// Returns None if the sequence is empty
  let inline tryReduce f xs = FiniteSeq.tryReduce f xs

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  let inline map f xs = FiniteSeq.map f xs

  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  let inline mapi f xs = FiniteSeq.mapi f xs

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.
  let inline map2 f xs ys = FiniteSeq.map2 f xs ys

  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  let inline filter f xs = FiniteSeq.filter f xs

  /// Wraps the two given enumerations as a single concatenated enumeration.
  let inline append xs ys = FiniteSeq.append xs ys

  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  let inline concat xs = FiniteSeq.concat xs

  /// O(n), where n is count. Return the list which on consumption will remove of at most 'n' elements of
  /// the input list.
  let inline drop n xs = FiniteSeq.drop n xs

  /// Views the given array as a finite sequence.
  let inline ofArray xs = FiniteSeq.ofArray xs

  /// Views the given seq as a finite sequence.  There is no runtime validation
  /// that the seq is actually finite, so this is a programmer assertion that the
  /// seq will be finite.
  let inline ofSeq xs = FiniteSeq.ofSeq xs 

  /// Views the given list as a finite sequence.  
  let inline ofList xs = FiniteSeq.ofList xs

  /// Returns a new sequence with the elements in reverse order.
  let inline rev xs = FiniteSeq.rev xs

  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  let inline scan f initialState xs = FiniteSeq.scan f initialState xs

  /// Builds an array from the given collection.
  let inline toArray xs = FiniteSeq.toArray xs

  /// Builds a List from the given collection.
  let inline toList xs = FiniteSeq.toList xs

  /// Views the given FiniteSeq as a sequence.
  let inline toSeq xs = FiniteSeq.toSeq

  /// Returns a sequence that when enumerated returns at most N elements.
  let inline truncate n xs = FiniteSeq.truncate n xs

  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  let inline tryFind predicate xs = FiniteSeq.tryFind predicate xs

  /// Returns the first element of the sequence.
  let inline tryHead xs = FiniteSeq.tryHead xs

  /// Returns the first element of the sequence.
  let inline headSafe xs = FiniteSeq.headSafe xs

  /// Returns the first element of the sequence.
  let inline head' xs = FiniteSeq.head' xs

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns None if the sequences are different lengths
  let inline tryMap2 f xs ys = FiniteSeq.tryMap2 f xs ys 

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline map2Safe f xs ys = FiniteSeq.map2Safe f xs ys

  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline map2' f xs ys = FiniteSeq.map2' f xs ys

  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  let inline trySkip n xs = FiniteSeq.trySkip n xs

  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  let inline skipSafe n xs = FiniteSeq.skipSafe n xs

  /// O(n), where n is count. Return the list which skips the first 'n' elements of
  /// the input list.
  let inline skip' n xs = FiniteSeq.skip' n xs

  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let inline tryTail xs = FiniteSeq.tryTail xs

  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let inline tailSafe xs = FiniteSeq.tailSafe xs

  /// O(1). Return option the list corresponding to the remaining items in the sequence.
  /// Forces the evaluation of the first cell of the list if it is not already evaluated.
  let inline tail' xs = FiniteSeq.tail' xs

  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let inline tryTake n xs = FiniteSeq.tryTake n xs
    
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let inline takeSafe n xs = FiniteSeq.takeSafe n xs
    
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  let inline take' n xs = FiniteSeq.take' n xs

  /// O(1). Returns tuple of head element and tail of the list.
  let inline tryUncons xs = FiniteSeq.tryUncons xs

  /// O(1). Returns tuple of head element and tail of the list.
  let inline unconsSafe xs = FiniteSeq.unconsSafe xs
  
  /// O(1). Returns tuple of head element and tail of the list.
  let inline uncons' xs = FiniteSeq.uncons' xs

  /// Combines the two sequences into a list of pairs. 
  /// Returns None if the sequences are different lengths
  let inline tryZip xs ys = FiniteSeq.tryZip xs ys

  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline zipSafe xs ys = FiniteSeq.zipSafe xs ys

  /// Combines the two sequences into a list of pairs. 
  /// Returns a DifferingLengths Error if the sequences are different lengths.
  let inline zip' xs ys = FiniteSeq.zip' xs ys

  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  let inline zip xs ys = FiniteSeq.zip xs ys
  
