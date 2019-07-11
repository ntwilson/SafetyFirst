module SafetyFirst.Seq

open SafetyFirst.ErrorTypes
open SafetyFirst.Numbers

/// <summary>
/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than or equal to zero.
/// </summary>
let chunkBySizeSafe size xs =
  if size <= 0 
  then Error chunkErr
  else Ok <| Seq.chunkBySize size xs

/// <summary>
/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than or equal to zero.
/// </summary>
let inline chunkBySize' size xs = chunkBySizeSafe size xs

/// <summary>
/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Same as <c>Seq.chunkBySize</c>, but restricts the input to a PositiveInt
/// </summary>
let chunksOf (PositiveInt size) xs = Seq.chunkBySize size xs

/// <summary>
/// If the input sequence has only one element, returns that element.
/// If the input sequence has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let exactlyOneSafe xs =
  let xs = Seq.cache (Seq.truncate 2 xs)
  match Seq.length xs with
  | 1 -> Ok <| Seq.head xs
  | 0 -> Error lessThanOneErr
  | _ -> Error moreThanOneErr 

/// <summary>
/// If the input sequence has only one element, returns that element.
/// If the input sequence has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let inline exactlyOne' xs = exactlyOneSafe xs

/// <summary>
/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
/// </summary>
let findSafe predicate xs = 
  Seq.tryFind predicate xs 
  |> Result.ofOption findErr

/// <summary>
/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
/// </summary>
let inline find' predicate xs = findSafe predicate xs

/// <summary>
/// Returns the index of the first element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findIndexSafe predicate xs = 
  Seq.tryFindIndex predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the index of the first element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findIndex' predicate xs = findIndexSafe predicate xs

/// <summary>
/// Returns the first element of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let headSafe xs =
  if Seq.isEmpty xs 
  then Error headErr
  else Ok <| Seq.head xs

/// <summary>
/// Returns the first element of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline head' xs = headSafe xs

/// <summary>
/// Generates a new sequence which, when iterated, will return successive elements by calling the given function, up to the given count.
/// Same as <c>Seq.init</c>, but restricts <c>count</c> to a NaturalInt, and provides NaturalInt indices to <c>initializer</c>.
/// </summary>
let initN (count:NaturalInt) initializer = Seq.init count.Value (initializer << (NaturalInt.verify >> Option.unless "F# core assumption failed: Seq.init called an initializer with a negative index."))

/// <summary>
/// Generates a new sequence which, when iterated, will return successive elements by calling the given function, up to the given count.
/// Returns a NegativeInput Error when <c>count</c> is not natural.
/// </summary>
let initSafe count initializer =
  match count with
  | NonNatural _ -> Error <| initErr count
  | Natural count -> Ok <| initN count initializer

/// <summary>
/// Generates a new sequence which, when iterated, will return successive elements by calling the given function, up to the given count.
/// Returns a NegativeInput Error when <c>count</c> is not natural.
/// </summary>
let inline init' count initializer = initSafe count initializer

/// <summary>
/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
/// </summary>
let itemSafe index xs = 
  let xs = ResizeArray (Seq.truncate (index + 1) xs)
  if index >= 0
  then 
    if Seq.length xs > index 
    then Ok <| xs.[index]
    else Error <| lazyIndexTooLargeErr index
  else Error <| indexNegativeErr index

/// <summary>
/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
/// </summary>
let inline item' index xs = itemSafe index xs

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let pickSafe chooser xs = 
  Seq.tryPick chooser xs
  |> Result.ofOption pickErr

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline pick' chooser xs = pickSafe chooser xs

/// <summary>
/// Generates a new sequence which, when iterated, will return the given value for every element, up to the given count.
/// Same as <c>Seq.replicate</c>, but restricts <c>count</c> to a NaturalInt.
/// </summary>
let replicateN (count:NaturalInt) initial = Seq.replicate count.Value initial

/// <summary>
/// Generates a new sequence which, when iterated, will return the given value for every element, up to the given count.
/// Returns a NegativeInput Error when <c>count</c> is not natural.
/// </summary>
let replicateSafe count initial =
  match count with
  | NonNatural _ -> Error <| replicateErr count
  | Natural count -> Ok <| replicateN count initial

/// <summary>
/// Generates a new sequence which, when iterated, will return the given value for every element, up to the given count.
/// Returns a NegativeInput Error when <c>count</c> is not natural.
/// </summary>
let inline replicate' count initial = replicateSafe count initial

/// <summary>
/// Returns a sequence that skips N elements of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// NOTE: This eagerly evaluates the skipped elements to ensure there are enough elements,
/// as opposed to the unsafe Seq.skip, which lazily evaluates and will throw as you iterate it.
/// NOTE: This evaluates the skipped elements twice: once to ensure there are enough elements,
/// and a second time to produce the result.  This is necessary because caching the sequence 
/// would make it no longer memory-safe for use with infinite sequences.  If the input sequence
/// is expensive to compute but finite, it is recommended you cache it with Seq.cache before
/// calling this function.
/// </summary>
let skipSafe count (xs:_ seq) =
  if (Seq.length (Seq.truncate count xs) = count) || count < 0
  then Ok <| Seq.skip count xs
  else Error <| lazySkipErr count 

/// <summary>
/// Returns a sequence that skips N elements of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// NOTE: This eagerly evaluates the skipped elements to ensure there are enough elements,
/// as opposed to the unsafe Seq.skip, which lazily evaluates and will throw as you iterate it.
/// NOTE: This evaluates the skipped elements twice: once to ensure there are enough elements,
/// and a second time to produce the result.  This is necessary because caching the sequence 
/// would make it no longer memory-safe for use with infinite sequences.  If the input sequence
/// is expensive to compute but finite, it is recommended you cache it with Seq.cache before
/// calling this function.
/// </summary>
let inline skip' count xs = skipSafe count xs

/// <summary>
/// Returns a sequence that skips at least N elements of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns an empty sequence if <c>count</c> exceeds the length of <c>xs</c> 
/// NOTE: This eagerly evaluates the skipped elements to ensure there are enough elements,
/// as opposed to the unsafe Seq.skip, which lazily evaluates and will throw as you iterate it.
/// NOTE: This evaluates the skipped elements twice: once to ensure there are enough elements,
/// and a second time to produce the result.  This is necessary because caching the sequence 
/// would make it no longer memory-safe for use with infinite sequences.  If the input sequence
/// is expensive to compute but finite, it is recommended you cache it with Seq.cache before
/// calling this function.
/// </summary>
let skipLenient count xs = 
  skip' count xs
  |> Result.defaultValue Seq.empty

/// <summary>
/// Returns a sequence that skips 1 element of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let tailSafe xs = 
  if Seq.isEmpty xs 
  then Error <| tailErr
  else Ok <| Seq.tail xs

/// <summary>
/// Returns a sequence that skips 1 element of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let inline tail' xs = tailSafe xs

/// <summary>
/// Returns the first N elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let takeSafe count xs = 
  let xs = ResizeArray (Seq.truncate count xs)
  if Seq.length xs = count 
  then Ok <| seq xs
  else Error <| lazyTakeErr count

/// <summary>
/// Returns the first N elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let inline take' count xs = takeSafe count xs

/// <summary>
/// Returns a sequence that yields sliding windows containing elements drawn from the input
/// sequence. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let windowedSafe size xs = 
  if size > 0 
  then Ok <| Seq.windowed size xs
  else Error <| windowedErr size

/// <summary>
/// Returns a sequence that yields sliding windows containing elements drawn from the input
/// sequence. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let inline windowed' size xs = windowedSafe size xs

/// <summary>
/// Returns a sequence that yields sliding windows containing elements drawn from the input
/// sequence. Each window is returned as a fresh array.
/// Same as <c>Seq.windowed</c> but restricts the input to a PositiveInt
/// </summary>
let window (PositiveInt size) xs = Seq.windowed size xs

/// <summary>
/// Functions for manipulating NonEmpty Seqs 
/// </summary>
module NonEmpty =

  /// <summary>
  /// Creates a new NonEmptySeq with the provided head and tail.  
  /// </summary>
  let create head tail : NonEmptySeq<_> = NonEmpty (Seq.append [head] tail)

  /// <summary>
  /// Returns a sequence that yields one item only.
  /// </summary>
  let singleton head : NonEmptySeq<_> = NonEmpty (Seq.singleton head)
 
  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let head (NonEmpty xs) = Seq.head xs

  /// <summary>
  /// Returns the sequence after removing the first element.
  /// </summary>
  let tail (NonEmpty xs) = Seq.tail xs
  
  /// <summary>
  /// Returns the tuple of the sequence's head and tail
  /// </summary>
  let uncons xs = (head xs, tail xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
  /// </summary>
  let indexed (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.indexed xs)

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive elements by calling the given function.
  /// Same as <c>Seq.initInfinite</c>, but provides NaturalInt indices to <c>initializer</c>.
  /// </summary>
  let initInfinitely initializer = 
    create 
      (initializer NaturalInt.zero) 
      (Seq.initInfinite (
        NaturalInt.verify
        >> Option.unless "F# core assumption failed: Seq.initInfinite called an initializer with a negative index."
        >> NaturalInt.increment 
        >> PositiveInt.asNatural 
        >> initializer
      ))

  /// <summary>
  /// Generates a new sequence which, when iterated, will return successive elements by calling the given function, up to the given count.
  /// Same as <c>Seq.init</c>, but restricts <c>count</c> to a PositiveInt, and provides NaturalInt indices to <c>initializer</c>.
  /// </summary>
  let initN (count:PositiveInt) initializer = create (initializer NaturalInt.zero) (initN count.Decrement (NaturalInt.increment >> PositiveInt.asNatural >> initializer))

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let map f (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.map f xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (NonEmpty xs) : NonEmptySeq<_> =
    NonEmpty (Seq.mapi f xs)
  
  /// <summary>
  /// O(1). Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2 f (NonEmpty xs) (NonEmpty ys) : NonEmptySeq<_> =
    NonEmpty (Seq.map2 f xs ys)

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let filter f (NonEmpty xs) = Seq.filter f xs

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let append (NonEmpty xs : NonEmptySeq<_>) (NonEmpty ys : NonEmptySeq<_>) : NonEmptySeq<_> = NonEmpty (Seq.append xs ys)

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let appendL (NonEmpty xs : NonEmptySeq<_>) ys : NonEmptySeq<_> = NonEmpty (Seq.append xs ys)

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let appendR xs (NonEmpty ys : NonEmptySeq<_>) : NonEmptySeq<_> = NonEmpty (Seq.append xs ys)

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let concat (NonEmpty xs : NonEmptySeq<NonEmptySeq<'a>>) : NonEmptySeq<_>= 
    NonEmpty (Seq.concat xs)

  /// <summary>
  /// Applies the given function to each element of the sequence and concatenates all the results.
  /// Returned sequence is lazy, effects are delayed until it is enumerated.
  /// </summary>
  let collect (f : 'a -> NonEmptySeq<'b>) (NonEmpty xs : NonEmptySeq<'a>) : NonEmptySeq<'b> = 
    NonEmpty (Seq.collect f xs)

  /// <summary>
  /// Returns a sequence that contains no duplicate entries according to generic hash and equality comparisons on the entries. 
  /// If an element occurs multiple times in the sequence then the later occurrences are discarded.
  /// </summary>
  let distinct (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.distinct xs)

  /// <summary>
  /// Returns a sequence that contains no duplicate entries according to the generic hash and equality comparisons 
  /// on the keys returned by the given key-generating function. 
  /// If an element occurs multiple times in the sequence then the later occurrences are discarded.
  /// </summary>
  let distinctBy projection (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.distinctBy projection xs) 

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofSeqSafe (xs:_ seq) : Result<NonEmptySeq<_>,_> = 
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
  let pairwise (NonEmpty xs) = Seq.pairwise xs

  /// <summary>
  /// Generates a new sequence which, when iterated, will return the given value for every element.
  /// </summary>
  let replicateInfinitely initial = create initial (Seq.initInfinite (fun _ -> initial))

  /// <summary>
  /// Generates a new sequence which, when iterated, will return the given value for every element, up to the given count.
  /// Same as <c>Seq.replicate</c>, but restricts <c>count</c> to a PositiveInt.
  /// </summary>
  let replicateN (count:PositiveInt) initial = create initial (replicateN count.Decrement initial)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.scan f initialState xs)

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let toArray (NonEmpty xs) = Seq.toArray xs

  /// <summary>
  /// Builds a NonEmpty array from the given collection.
  /// </summary>
  let toNonEmptyArray xs : NonEmptyArray<_> = NonEmpty <| toArray xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let toList (NonEmpty xs) = Seq.toList xs

  /// <summary>
  /// Builds a NonEmpty List from the given collection.
  /// </summary>
  let toNonEmptyList xs : NonEmptyList<_> = NonEmpty <| toList xs

  /// <summary>
  /// Views the given NonEmptySeq as a sequence.
  /// </summary>
  let toSeq (NonEmpty xs) : _ seq = upcast xs 

  /// <summary>
  /// Views the given NonEmptySeq as a NonEmpty FSeq.
  /// </summary>
  let toNonEmptyFSeq (NonEmpty xs) : NonEmptyFSeq<_> = NonEmpty <| fseq xs

  /// <summary>
  /// Returns a sequence that when enumerated returns at most n elements.
  /// </summary>
  let truncate (PositiveInt n) (NonEmpty xs) : NonEmptySeq<_> = NonEmpty (Seq.truncate n xs)

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let tryFind predicate (NonEmpty xs) = Seq.tryFind predicate xs

  /// <summary>
  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let trySkip n (NonEmpty xs) = Result.toOption <| skip' n xs 

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let tryTake n (NonEmpty xs) = Result.toOption <| take' n xs

  /// <summary>
  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let zip (NonEmpty xs) (NonEmpty ys) : NonEmptySeq<_> = NonEmpty (Seq.zip xs ys)

  /// <summary>
  /// Splits a sequence at every occurrence of an element satisfying <c>splitAfter</c>.
  /// The split occurs immediately after each element that satisfies <c>splitAfter</c>,
  /// and the element satisfying <c>splitAfter</c> will be included as the last element of 
  /// the sequence preceeding the split.
  /// For example:
  /// <code>
  /// split ((=) 100) (Seq.NonEmpty.create 1[2;3;100;100;4;100;5;6])
  ///   //returns ([[1;2;3;100];[100];[4;100];[5;6]])
  /// </code>
  /// </summary>
  let split splitAfter xs : NonEmptySeq<_> = 
    let (++) (NonEmpty xs) ys = NonEmpty <| Seq.append xs ys
  
    let takeGroup input : NonEmptySeq<_> =
      let rec takeGroup' input =  
        seq { 
          match input with
          | SeqOneOrMore (head, tail) ->
            if splitAfter head 
            then yield head
            else 
              yield head
              yield! takeGroup' tail
          | _ -> ()
        }

      let head, tail = uncons input
      if splitAfter head 
      then singleton head
      else singleton head ++ takeGroup' tail

    let rec split' (xs:NonEmptySeq<_>) = 
      NonEmpty (
        seq { 
          yield takeGroup xs

          let subsequentElements = Seq.skipWhile (not << splitAfter) xs |> skipLenient 1
          match subsequentElements with
          | Empty -> ()
          | NotEmpty elements -> yield! split' elements
        }
      )

    split' (NonEmpty <| toSeq xs)

  /// <summary>
  /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
  /// For example:
  /// <code>
  /// splitPairwise (=) (Seq.NonEmpty.create 0[1;1;2;3;4;4;4;5])
  ///   //returns [[0;1];[1;2;3;4];[4];[4;5]]
  /// </code>
  /// </summary>
  let rec splitPairwise splitBetween xs : NonEmptySeq<_> =
    let (++) (NonEmpty xs) ys = NonEmpty <| Seq.append xs ys

    let takeGroup input : NonEmptySeq<_> =
      let rec takeGroup' previousElement input =  
        seq { 
          match input with
          | SeqOneOrMore (head, tail) ->
            if not <| splitBetween previousElement head 
            then 
              yield head
              yield! takeGroup' head tail
          | _ -> ()
        }

      let head, tail = uncons input
      singleton head ++ takeGroup' head tail 

    let rec skipGroup input =
      let firstElement, tail = uncons input
      match tail with
      | Empty -> Seq.empty
      | NotEmpty tail ->
        let secondElement = head tail
        if splitBetween firstElement secondElement 
        then seq tail
        else skipGroup tail

    let rec splitPairwise' xs = 
      NonEmpty (
        seq { 
          yield takeGroup xs

          match skipGroup xs with
          | Empty -> ()
          | NotEmpty elements -> yield! splitPairwise' elements
        }
      )

    splitPairwise' (NonEmpty <| toSeq xs)    
