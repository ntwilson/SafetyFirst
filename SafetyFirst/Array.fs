module SafetyFirst.Array

open ResultDotNet.FSharp

open SafetyFirst.ErrorTypes
open SafetyFirst.Numbers

/// <summary>
/// Returns the average of the elements in the array.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
/// </summary>
let inline averageSafe source = 
  if Array.isEmpty source
  then Error <| avgErr ()
  else Ok <| Array.average source

/// <summary>
/// Returns the average of the elements in the array.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
/// </summary>
let inline average' source = averageSafe source 

/// <summary>
/// Returns the average of the results generated by applying the function to each element
/// of the array.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline averageBySafe selector xs = 
  if Array.isEmpty xs
  then Error <| avgErr ()
  else Ok <| Array.averageBy selector xs

/// <summary>
/// Returns the average of the results generated by applying the function to each element
/// of the array.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline averageBy' selector xs = averageBySafe selector xs

/// <summary>
/// Divides the input array into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
/// </summary>
let chunkBySizeSafe size xs =
  if size <= 0 
  then Error chunkErr
  else Ok <| Array.chunkBySize size xs

/// <summary>
/// Divides the input array into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
/// </summary>
let inline chunkBySize' size xs = chunkBySizeSafe size xs

/// <summary>
/// Divides the input array into chunks of size at most <c>size</c>.
/// Same as <c>Array.chunkBySize</c>, but restricts the input to a PositiveInt
/// </summary>
let chunksOf (PositiveInt size) xs = Array.chunkBySize size xs

/// <summary>
/// If the input array has only one element, returns that element.
/// If the input array has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let exactlyOneSafe xs =
  match Array.length xs with
  | 1 -> Ok <| Array.head xs
  | 0 -> Error lessThanOneErr
  | _ -> Error moreThanOneErr 

/// <summary>
/// If the input array has only one element, returns that element.
/// If the input array has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let inline exactlyOne' xs = exactlyOneSafe xs

/// <summary>
/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
/// </summary>
let findSafe predicate xs = 
  Array.tryFind predicate xs 
  |> Result.ofOption findErr

/// <summary>
/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
/// </summary>
let inline find' predicate xs = findSafe predicate xs

/// <summary>
/// Returns the last element for which the given function returns True.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findBackSafe predicate xs = 
  Array.tryFindBack predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the last element for which the given function returns True.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findBack' predicate xs = findBackSafe predicate xs

/// <summary>
/// Returns the index of the first element in the array
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findIndexSafe predicate xs = 
  Array.tryFindIndex predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the index of the first element in the array
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findIndex' predicate xs = findIndexSafe predicate xs

/// <summary>
/// Returns the index of the last element in the array
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findIndexBackSafe predicate xs = 
  Array.tryFindIndexBack predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the index of the last element in the array
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findIndexBack' predicate xs = findIndexBackSafe predicate xs

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c>
/// then computes <c>f (... (f s i0 j0)...) iN jN</c>.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let fold2Safe folder initialState xs ys = 
  if Array.length xs = Array.length ys
  then Ok <| Array.fold2 folder initialState xs ys
  else Error <| fold2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c>
/// then computes <c>f (... (f s i0 j0)...) iN jN</c>.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline fold2' folder initialState xs ys = fold2Safe folder initialState xs ys

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c>
/// then computes <c>f i0 j0 (...(f iN jN s))</c>.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let foldBack2Safe folder xs ys initialState = 
  if Array.length xs = Array.length ys
  then Ok <| Array.foldBack2 folder xs ys initialState 
  else Error <| fold2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c>
/// then computes <c>f i0 j0 (...(f iN jN s))</c>.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline foldBack2' folder xs ys initialState = foldBack2Safe folder xs ys initialState

/// <summary>
/// Tests if all corresponding elements of the array satisfy the given predicate pairwise.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let forall2Safe predicate xs ys = 
  if Array.length xs = Array.length ys
  then Ok <| Array.forall2 predicate xs ys
  else Error <| forall2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Tests if all corresponding elements of the array satisfy the given predicate pairwise.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline forall2' predicate xs ys = forall2Safe predicate xs ys

/// <summary>
/// Returns the first element of the array.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let headSafe xs =
  if Array.isEmpty xs 
  then Error headErr
  else Ok <| Array.head xs

/// <summary>
/// Returns the first element of the array.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline head' xs = headSafe xs

/// <summary>
/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
/// </summary>
let itemSafe index xs = 
  if index < 0
    then Error <| indexNegativeErr index
  elif Array.length xs <= index
    then Error <| indexTooLargeErr index (Array.length xs)
  else 
    Ok <| xs.[index]  

/// <summary>
/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
/// </summary>
let inline item' index xs = itemSafe index xs

/// <summary>
/// Applies the given function to two collections simultaneously. The collections must have identical size.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let iter2Safe action xs ys = 
  if Array.length xs = Array.length ys 
  then Ok <| Array.iter2 action xs ys
  else Error <| iter2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Applies the given function to two collections simultaneously. The collections must have identical size.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline iter2' action xs ys = iter2Safe action xs ys

/// <summary>
/// Applies the given function to two collections simultaneously. The
/// collections must have identical size. The integer passed to the
/// function indicates the index of element.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let iteri2Safe action xs ys = 
  if Array.length xs = Array.length ys 
  then Ok <| Array.iteri2 action xs ys
  else Error <| iteri2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Applies the given function to two collections simultaneously. The
/// collections must have identical size. The integer passed to the
/// function indicates the index of element.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline iteri2' action xs ys = iteri2Safe action xs ys 

/// <summary>
/// Returns the last element of the array.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let lastSafe xs = Array.tryLast xs |> Result.ofOption lastErr  

/// <summary>
/// Returns the last element of the array.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline last' xs = lastSafe xs

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let map2Safe f xs ys = 
  if Array.length xs = Array.length ys 
  then Ok <| Array.map2 f xs ys
  else Error <| map2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline map2' f xs ys = map2Safe f xs ys

/// <summary>
/// Like mapi, but mapping corresponding elements from two arrays of equal length.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let mapi2Safe f xs ys =
  if Array.length xs = Array.length ys 
  then Ok <| Array.mapi2 f xs ys
  else Error <| mapi2Err (Array.length xs) (Array.length ys)

/// <summary>
/// Like mapi, but mapping corresponding elements from two arrays of equal length.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline mapi2' f xs ys = mapi2Safe f xs ys

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the three collections simultaneously.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let map3Safe f xs ys zs = 
  if Array.length xs = Array.length ys && Array.length xs = Array.length zs
  then Ok <| Array.map3 f xs ys zs
  else Error <| map3Err (Array.length xs) (Array.length ys) (Array.length zs)

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the three collections simultaneously.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline map3' f xs ys zs = map3Safe f xs ys zs

/// <summary>
/// Returns the greatest of all elements of the array, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let maxSafe<'a when 'a : comparison> (xs:'a array) =
  if Array.isEmpty xs 
  then Error <| maxErr
  else Ok <| Array.max xs

/// <summary>
/// Returns the greatest of all elements of the array, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline max'<'a when 'a : comparison> (xs:'a array) = maxSafe xs

/// <summary>
/// Returns the greatest of all elements of the array, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let maxBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a array) =
  if Array.isEmpty xs
  then Error <| maxErr
  else Ok <| Array.maxBy projection xs

/// <summary>
/// Returns the greatest of all elements of the array, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline maxBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a array) = maxBySafe projection xs

/// <summary>
/// Returns the lowest of all elements of the array, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let minSafe<'a when 'a : comparison> (xs:'a array) =
  if Array.isEmpty xs 
  then Error <| minErr
  else Ok <| Array.min xs

/// <summary>
/// Returns the lowest of all elements of the array, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline min'<'a when 'a : comparison> (xs:'a array) = minSafe xs

/// <summary>
/// Returns the lowest of all elements of the array, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let minBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a array) =
  if Array.isEmpty xs 
  then Error <| minErr
  else Ok <| Array.minBy projection xs

/// <summary>
/// Returns the lowest of all elements of the array, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline minBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a array) = minBySafe projection xs

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let pickSafe chooser xs = 
  Array.tryPick chooser xs
  |> Result.ofOption pickErr

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline pick' chooser xs = pickSafe chooser xs

/// <summary>
/// Applies a function to each element of the array, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// Returns a SeqIsEmpty Error if the array is empty.
/// </summary>
let reduceSafe reduction xs = 
  if Array.isEmpty xs
  then Error <| reduceErr
  else Ok <| Array.reduce reduction xs

/// <summary>
/// Applies a function to each element of the array, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// Returns a SeqIsEmpty Error if the array is empty.
/// </summary>
let inline reduce' reduction xs = reduceSafe reduction xs

/// <summary>
/// Applies a function to each element of the array, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the array is empty.
/// </summary>
let reduceBackSafe reduction xs = 
  if Array.isEmpty xs
  then Error <| reduceErr
  else Ok <| Array.reduceBack reduction xs

/// <summary>
/// Applies a function to each element of the array, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the array is empty.
/// </summary>
let inline reduceBack' reduction xs = reduceBackSafe reduction xs 

/// <summary>
/// Returns an array that skips N elements of the underlying array and then yields the
/// remaining elements of the array.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let skipSafe count xs =
  if (Array.length xs >= count) || count < 0
  then Ok <| Array.skip count xs
  else Error <| lazySkipErr count 

/// <summary>
/// Returns an array that skips N elements of the underlying array and then yields the
/// remaining elements of the array.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let inline skip' count xs = skipSafe count xs

/// <summary>
/// Returns an array that skips at least N elements of the underlying array and then yields the
/// remaining elements of the array.
/// Returns an empty array if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let skipLenient count xs = 
  skip' count xs
  |> Result.defaultValue [||]

/// <summary>
/// Splits an array into two arrays, at the given index.
/// Returns an IndexOutOfBounds Error when split index exceeds 
/// the number of elements in the array.
/// </summary>
let splitAtSafe index xs = 
  if index < 0 
    then Error <| indexNegativeErr index
  elif index > Array.length xs
    then Error <| indexTooLargeErr index (Array.length xs)
  else
    Ok <| Array.splitAt index xs

/// <summary>
/// Splits an array into two arrays, at the given index.
/// Returns an IndexOutOfBounds Error when split index exceeds 
/// the number of elements in the array.
/// </summary>
let inline splitAt' index xs = splitAtSafe index xs 

/// <summary>
/// Splits the input array into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
/// </summary>
let splitIntoSafe count xs = 
  if count > 0 
  then Ok <| Array.splitInto count xs
  else Error <| splitIntoErr count

/// <summary>
/// Splits the input array into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
/// </summary>
let inline splitInto' count xs = splitIntoSafe count xs

/// <summary>
/// Splits the input array into at most count chunks.
/// Same as <c>Array.splitInto</c>, but restricts the input to a PositiveInt
/// </summary>
let splitIntoN (PositiveInt count) xs = Array.splitInto count xs

/// <summary>
/// Slices an array given a starting index and a count of elements to return.
/// Returns an IndexOutOfBounds Error if either <c>startIndex</c> or <c>count</c> is negative,
/// or if there aren't enough elements in the input array.
/// (This is computed if the <c>startIndex</c> + the <c>count</c> is greater
/// than the length of the array.  Note that it returns an empty array if <c>startIndex</c>
/// is equal to the length of the array and the <c>count</c> is 0.)
/// </summary>
let subSafe xs startIndex count = 
  if startIndex < 0 then Error <| namedIndexNegativeErr "startIndex" startIndex
  elif count < 0 then Error <| namedIndexNegativeErr "count" count
  elif startIndex + count > Array.length xs then Error <| subErr startIndex count (Array.length xs)
  else Ok <| Array.sub xs startIndex count

/// <summary>
/// Slices an array given a starting index and a count of elements to return.
/// Returns an IndexOutOfBounds Error if either <c>startIndex</c> or <c>count</c> is negative,
/// or if there aren't enough elements in the input array.
/// (This is computed if the <c>startIndex</c> + the <c>count</c> is greater
/// than the length of the array.  Note that it returns an empty array if <c>startIndex</c>
/// is equal to the length of the array and the <c>count</c> is 0.)
/// </summary>
let inline sub' xs startIndex count = subSafe xs startIndex count

/// <summary>
/// Returns an array that skips 1 element of the underlying array and then yields the
/// remaining elements of the array.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let tailSafe xs = 
  if Array.isEmpty xs 
  then Error <| tailErr
  else Ok <| Array.tail xs

/// <summary>
/// Returns an array that skips 1 element of the underlying array and then yields the
/// remaining elements of the array.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let inline tail' xs = tailSafe xs

/// <summary>
/// Returns the first N elements of the array.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let takeSafe count xs = 
  if Array.length xs >= count && count >= 0
  then Ok <| Array.take count xs
  else Error <| lazyTakeErr count

/// <summary>
/// Returns the first N elements of the array.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let inline take' count xs = takeSafe count xs

/// <summary>
/// Returns an array that yields sliding windows containing elements drawn from the input
/// array. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let windowedSafe size xs = 
  if size > 0 
  then Ok <| Array.windowed size xs
  else Error <| windowedErr size

/// <summary>
/// Returns an array that yields sliding windows containing elements drawn from the input
/// array. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let inline windowed' size xs = windowedSafe size xs

/// <summary>
/// Returns an array that yields sliding windows containing elements drawn from the input
/// array. Each window is returned as a fresh array.
/// Same as <c>Array.windowed</c>, but restricts the input to a PositiveInt
/// </summary>
let window (PositiveInt size) xs = Array.windowed size xs

/// <summary>
/// Combines the two arrays into an array of pairs. The two arrays must have equal lengths.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let zipSafe xs ys = 
  if Array.length xs = Array.length ys
  then Ok <| Array.zip xs ys 
  else Error <| zipErr (Array.length xs) (Array.length ys)

/// <summary>
/// Combines the two arrays into an array of pairs. The two arrays must have equal lengths.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline zip' xs ys = zipSafe xs ys

/// <summary>
/// Combines the three arrays into an array of triples. The arrays must have equal lengths.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let zip3Safe xs ys zs = 
  if Array.length xs = Array.length ys && Array.length xs = Array.length zs
  then Ok <| Array.zip3 xs ys zs
  else Error <| zip3Err (Array.length xs) (Array.length ys) (Array.length zs)

/// <summary>
/// Combines the three arrays into an array of triples. The arrays must have equal lengths.
/// Returns a DifferingLengths Error if the input arrays have a different number of elements.
/// </summary>
let inline zip3' xs ys zs = zip3Safe xs ys zs

//TODO: transpose, split once we use a newer FSharp.Core


/// <summary>
/// Functions for manipulating NonEmpty Arrays
/// </summary>
module NonEmpty =

  /// <summary>
  /// Creates a new NonEmptySeq with the provided head and tail.  
  /// The tail is constrained to be finite.  If the tail is infinite,
  /// use Seq.NonEmpty.create instead
  /// </summary>
  let create head tail : NonEmptyArray<_> = NonEmpty (Array.append [|head|] tail)

  /// <summary>
  /// Returns a NonEmpty array that contains one item only.
  /// </summary>
  let singleton x : NonEmptyArray<_> = NonEmpty [|x|]

  /// <summary>
  /// Returns the first element of the sequence.
  /// </summary>
  let head (NonEmpty xs) = Array.head xs
  
  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.
  /// </summary
  let min (NonEmpty xs) = Array.min xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c>.
  /// </summary
  let max (NonEmpty xs) = Array.max xs

  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c> on the function result.
  /// </summary>
  let minBy projection (NonEmpty xs) = Array.minBy projection xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c> on the function result.
  /// </summary>
  let maxBy projection (NonEmpty xs) = Array.maxBy projection xs

  /// <summary>
  /// Returns the sequence after removing the first element.
  /// </summary>
  let tail (NonEmpty xs) = Array.tail xs
  
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
  let reduce f (NonEmpty xs) = Array.reduce f xs

  /// <summary>
  /// O(n). Returns the length of the sequence.
  /// </summary>
  let length (NonEmpty xs) = Array.length xs
  
  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let fold f initialState (NonEmpty xs) = Array.fold f initialState xs

  /// <summary>
  /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
  /// </summary>
  let indexed (NonEmpty xs) : NonEmptyArray<_> = NonEmpty (Array.indexed xs)

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let map f (NonEmpty xs) : NonEmptyArray<_> = NonEmpty (Array.map f xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (NonEmpty xs) : NonEmptyArray<_> =
    NonEmpty (Array.mapi f xs)
  
  let private (<!>) f x = Result.map f x

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2Safe f (NonEmpty xs) (NonEmpty ys) : Result<NonEmptyArray<_>,_> =
    NonEmpty <!> map2' f xs ys

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2' f (NonEmpty xs) (NonEmpty ys) : Result<NonEmptyArray<_>,_> = 
    NonEmpty <!> map2' f xs ys

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let filter f (NonEmpty xs) = Array.filter f xs

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let append xs (NonEmpty ys) : NonEmptyArray<_> = NonEmpty (Array.append xs ys)

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let concat (NonEmpty xs : NonEmptyArray<NonEmptyArray<'a>>) : NonEmptyArray<_> = 
    NonEmpty (xs |> Array.collect (fun (NonEmpty x) -> x))

  /// <summary>
  /// O(n), where n is count. Return the array which will remove at most 'n' elements of
  /// the input array.
  /// </summary>
  let drop (NaturalInt n) (NonEmpty xs) = 
    if n >= Array.length xs then [||]
    else xs.[n .. Array.length xs - 1]

  /// <summary>
  /// O(n), where n is count. Return the array which will remove at most 'n' elements of
  /// the input array.
  /// CAUTION: This function will THROW for negative values of 'n'.
  /// </summary>
  let dropUnsafe n (NonEmpty xs) = 
    match n with 
    | Natural _ ->
      if n >= Array.length xs then [||]
      else xs.[n .. Array.length xs - 1]
    | neg -> invalidArg "n" "Can't drop a negative number of values"

  /// <summary>
  /// O(n), where n is count. Return the array which will remove at most 'n' elements of
  /// the input array.
  /// This function will return the input array unaltered for negative values of 'n'.
  /// </summary>
  let dropLenient n (NonEmpty xs as arr) = 
    match n with
    | Natural i -> drop i arr
    | neg -> xs

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofArraySafe (xs:_[]) : Result<NonEmptyArray<_>,_> = 
    match xs with
    | Empty -> Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    | NotEmpty ys -> Ok <| ys

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofArray' xs = ofArraySafe xs

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (NonEmpty xs) = Array.pairwise xs

  /// <summary>
  /// Returns a new sequence with the elements in reverse order.
  /// </summary>
  let rev (NonEmpty xs) : NonEmptyArray<_> = NonEmpty (Array.rev xs)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (NonEmpty xs) : NonEmptyArray<_> = NonEmpty (Array.scan f initialState xs)

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let toArray (NonEmpty xs) = xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let toList (NonEmpty xs) = Array.toList xs

  /// <summary>
  /// Builds a NonEmpty List from the given collection.
  /// </summary>
  let toNonEmptyList xs : NonEmptyList<_> = NonEmpty <| toList xs

  /// <summary>
  /// Views the given NonEmptySeq as a sequence.
  /// </summary>
  let toSeq (NonEmpty xs : NonEmptyArray<_>) : _ seq = upcast xs 

  /// <summary>
  /// Views the given NonEmpty Array as a NonEmpty Seq.
  /// </summary>
  let toNonEmptySeq xs : NonEmptySeq<_> = NonEmpty <| toSeq xs

  /// <summary>
  /// Views the given NonEmpty Array as a NonEmpty FSeq
  /// </summary>
  let toNonEmptyFSeq xs : NonEmptyFSeq<_> = NonEmpty <| fseq xs

  /// <summary>
  /// Returns a NonEmpty Array that when enumerated returns at most n elements.
  /// </summary>
  let truncate (PositiveInt n) (NonEmpty xs) : NonEmptyArray<_> = 
    NonEmpty (Array.truncate n xs)

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let tryFind predicate (NonEmpty xs) = Array.tryFind predicate xs

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
  let zip' (NonEmpty xs) (NonEmpty ys) : Result<NonEmptyArray<_>,_> = 
    NonEmpty <!> zip' xs ys

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
  let split splitAfter xs = 
    FSeq.NonEmpty.split splitAfter (toNonEmptyFSeq xs)
    |> FSeq.NonEmpty.map FSeq.NonEmpty.toNonEmptyArray
    |> FSeq.NonEmpty.toNonEmptyArray

  /// <summary>
  /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
  /// For example:
  /// <code>
  /// splitPairwise (=) (Seq.NonEmpty.create 0[1;1;2;3;4;4;4;5])
  ///   //returns [[0;1];[1;2;3;4];[4];[4;5]]
  /// </code>
  /// </summary>
  let splitPairwise splitBetween xs =
    FSeq.NonEmpty.splitPairwise splitBetween (toNonEmptyFSeq xs)
    |> FSeq.NonEmpty.map FSeq.NonEmpty.toNonEmptyArray
    |> FSeq.NonEmpty.toNonEmptyArray
