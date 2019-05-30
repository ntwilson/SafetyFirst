module SafetyFirst.List

open SafetyFirst.ErrorTypes
open SafetyFirst.Numbers

/// <summary>
/// Returns the average of the elements in the list.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
/// </summary>
let inline averageSafe source = 
  if List.isEmpty source
  then Error <| avgErr ()
  else Ok <| List.average source

/// <summary>
/// Returns the average of the elements in the list.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
/// </summary>
let inline average' source = averageSafe source 

/// <summary>
/// Returns the average of the results generated by applying the function to each element
/// of the list.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline averageBySafe selector xs = 
  if List.isEmpty xs
  then Error <| avgErr ()
  else Ok <| List.averageBy selector xs

/// <summary>
/// Returns the average of the results generated by applying the function to each element
/// of the list.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let inline averageBy' selector xs = averageBySafe selector xs

/// <summary>
/// Divides the input list into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
/// </summary>
let chunkBySizeSafe size xs =
  if size <= 0 
  then Error chunkErr
  else Ok <| List.chunkBySize size xs

/// <summary>
/// Divides the input list into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
/// </summary>
let inline chunkBySize' size xs = chunkBySizeSafe size xs

/// <summary>
/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Same as <c>List.chunkBySize</c>, but restricts the input to a PositiveInt
/// </summary>
let chunksOf (PositiveInt size) xs = List.chunkBySize size xs

/// <summary>
/// If the input list has only one element, returns that element.
/// If the input list has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let exactlyOneSafe xs =
  match List.length xs with
  | 1 -> Ok <| List.head xs
  | 0 -> Error lessThanOneErr
  | _ -> Error moreThanOneErr 

/// <summary>
/// If the input list has only one element, returns that element.
/// If the input list has more or less than one element, returns a WrongNumberOfElements Error.  
/// </summary>
let inline exactlyOne' xs = exactlyOneSafe xs

/// <summary>
/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
/// </summary>
let findSafe predicate xs = 
  List.tryFind predicate xs 
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
  List.tryFindBack predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the last element for which the given function returns True.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findBack' predicate xs = findBackSafe predicate xs

/// <summary>
/// Returns the index of the first element in the list
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findIndexSafe predicate xs = 
  List.tryFindIndex predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the index of the first element in the list
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findIndex' predicate xs = findIndexSafe predicate xs

/// <summary>
/// Returns the index of the last element in the list
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let findIndexBackSafe predicate xs = 
  List.tryFindIndexBack predicate xs
  |> Result.ofOption findErr

/// <summary>
/// Returns the index of the last element in the list
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline findIndexBack' predicate xs = findIndexBackSafe predicate xs

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is f and the elements are i0...iN and j0...jN
/// then computes f (... (f s i0 j0)...) iN jN.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let fold2Safe folder initialState xs ys = 
  if List.length xs = List.length ys
  then Ok <| List.fold2 folder initialState xs ys
  else Error <| fold2Err (List.length xs) (List.length ys)

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is f and the elements are i0...iN and j0...jN
/// then computes f (... (f s i0 j0)...) iN jN.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline fold2' folder initialState xs ys = fold2Safe folder initialState xs ys

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is f and the elements are i0...iN and j0...jN
/// then computes f i0 j0 (...(f iN jN s)).
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let foldBack2Safe folder xs ys initialState = 
  if List.length xs = List.length ys
  then Ok <| List.foldBack2 folder xs ys initialState 
  else Error <| fold2Err (List.length xs) (List.length ys)

/// <summary>
/// Applies a function to corresponding elements of two collections, threading an accumulator argument
/// through the computation. The collections must have identical sizes.
/// If the input function is f and the elements are i0...iN and j0...jN
/// then computes f i0 j0 (...(f iN jN s)).
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline foldBack2' folder xs ys initialState = foldBack2Safe folder xs ys initialState

/// <summary>
/// Tests if all corresponding elements of the collection satisfy the given predicate pairwise.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// Note: if the predicate is proven false before done traversing the lists pairwise, will return 
/// <c>Ok false</c> even if the lists have different lengths.  This behavior mimics when 
/// <c>List.forall2</c> will throw.
/// </summary>
let forall2Safe predicate xs ys = 
  match (Seq.forall2 predicate xs ys, List.length xs = List.length ys) with
  | (false, _) -> Ok false
  | (true, true) -> Ok true
  | (true, false) -> Error <| forall2Err (List.length xs) (List.length ys)
  
/// <summary>
/// Tests if all corresponding elements of the collection satisfy the given predicate pairwise.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// Note: if the predicate is proven false before done traversing the lists pairwise, will return 
/// <c>Ok false</c> even if the lists have different lengths.  This behavior mimics when 
/// <c>List.forall2</c> will throw.
/// </summary>
let forall2' predicate xs ys = forall2Safe predicate xs ys

/// <summary>
/// Returns the first element of the list.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// </summary>
let headSafe xs =
  if List.isEmpty xs 
  then Error headErr
  else Ok <| List.head xs

/// <summary>
/// Returns the first element of the list.
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
  elif List.length xs <= index
    then Error <| indexTooLargeErr index (List.length xs)
  else 
    Ok <| xs.[index]  

/// <summary>
/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
/// </summary>
let inline item' index xs = itemSafe index xs

/// <summary>
/// Applies the given function to two collections simultaneously. The collections must have identical size.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let iter2Safe action xs ys = 
  if List.length xs = List.length ys 
  then Ok <| List.iter2 action xs ys
  else Error <| iter2Err (List.length xs) (List.length ys)

/// <summary>
/// Applies the given function to two collections simultaneously. The collections must have identical size.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline iter2' action xs ys = iter2Safe action xs ys

/// <summary>
/// Applies the given function to two collections simultaneously. The
/// collections must have identical size. The integer passed to the
/// function indicates the index of element.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let iteri2Safe action xs ys = 
  if List.length xs = List.length ys 
  then Ok <| List.iteri2 action xs ys
  else Error <| iteri2Err (List.length xs) (List.length ys)

/// <summary>
/// Applies the given function to two collections simultaneously. The
/// collections must have identical size. The integer passed to the
/// function indicates the index of element.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline iteri2' action xs ys = iteri2Safe action xs ys 

/// <summary>
/// Returns the last element of the list.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let lastSafe xs = List.tryLast xs |> Result.ofOption lastErr  

/// <summary>
/// Returns the last element of the list.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline last' xs = lastSafe xs

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let map2Safe f xs ys = 
  if List.length xs = List.length ys 
  then Ok <| List.map2 f xs ys
  else Error <| map2Err (List.length xs) (List.length ys)

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline map2' f xs ys = map2Safe f xs ys

/// <summary>
/// Like mapi, but mapping corresponding elements from two lists of equal length.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let mapi2Safe f xs ys =
  if List.length xs = List.length ys 
  then Ok <| List.mapi2 f xs ys
  else Error <| mapi2Err (List.length xs) (List.length ys)

/// <summary>
/// Like mapi, but mapping corresponding elements from two lists of equal length.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline mapi2' f xs ys = mapi2Safe f xs ys

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the three collections simultaneously.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let map3Safe f xs ys zs = 
  if List.length xs = List.length ys && List.length xs = List.length zs
  then Ok <| List.map3 f xs ys zs
  else Error <| map3Err (List.length xs) (List.length ys) (List.length zs)

/// <summary>
/// Builds a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the three collections simultaneously.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline map3' f xs ys zs = map3Safe f xs ys zs

/// <summary>
/// Returns the greatest of all elements of the list, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let maxSafe<'a when 'a : comparison> (xs:'a list) =
  if List.isEmpty xs 
  then Error <| maxErr
  else Ok <| List.max xs

/// <summary>
/// Returns the greatest of all elements of the list, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline max'<'a when 'a : comparison> (xs:'a list) = maxSafe xs

/// <summary>
/// Returns the greatest of all elements of the list, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let maxBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a list) =
  if List.isEmpty xs
  then Error <| maxErr
  else Ok <| List.maxBy projection xs

/// <summary>
/// Returns the greatest of all elements of the list, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline maxBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a list) = maxBySafe projection xs

/// <summary>
/// Returns the lowest of all elements of the list, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let minSafe<'a when 'a : comparison> (xs:'a list) =
  if List.isEmpty xs 
  then Error <| minErr
  else Ok <| List.min xs

/// <summary>
/// Returns the lowest of all elements of the list, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline min'<'a when 'a : comparison> (xs:'a list) = minSafe xs

/// <summary>
/// Returns the lowest of all elements of the list, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let minBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a list) =
  if List.isEmpty xs 
  then Error <| minErr
  else Ok <| List.minBy projection xs

/// <summary>
/// Returns the lowest of all elements of the list, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
/// </summary>
let inline minBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a list) = minBySafe projection xs

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let pickSafe chooser xs = 
  List.tryPick chooser xs
  |> Result.ofOption pickErr

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
/// </summary>
let inline pick' chooser xs = pickSafe chooser xs

/// <summary>
/// Applies a function to each element of the list, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (f (...(f i0 i1)) iN-1) iN</c>.
/// Returns a SeqIsEmpty Error if the list is empty.
/// </summary>
let reduceSafe reduction xs = 
  if List.isEmpty xs
  then Error <| reduceErr
  else Ok <| List.reduce reduction xs

/// <summary>
/// Applies a function to each element of the list, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (f (...(f i0 i1)) iN-1) iN</c>.
/// Returns a SeqIsEmpty Error if the list is empty.
/// </summary>
let inline reduce' reduction xs = reduceSafe reduction xs

/// <summary>
/// Applies a function to each element of the list, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the list is empty.
/// </summary>
let reduceBackSafe reduction xs = 
  if List.isEmpty xs
  then Error <| reduceErr
  else Ok <| List.reduceBack reduction xs

/// <summary>
/// Applies a function to each element of the list, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the list is empty.
/// </summary>
let inline reduceBack' reduction xs = reduceBackSafe reduction xs 

/// <summary>
/// Returns a list that skips N elements of the underlying list and then yields the
/// remaining elements of the list.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let skipSafe count xs =
  if (List.length xs >= count) || count < 0
  then Ok <| List.skip count xs
  else Error <| lazySkipErr count 

/// <summary>
/// Returns a list that skips N elements of the underlying list and then yields the
/// remaining elements of the list.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let inline skip' count xs = skipSafe count xs

/// <summary>
/// Returns a list that skips at least N elements of the underlying list and then yields the
/// remaining elements of the list.
/// Returns an empty list if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let skipLenient count xs = 
  skip' count xs
  |> Result.defaultValue []

/// <summary>
/// Splits a list into two lists, at the given index.
/// Returns an IndexOutOfBounds Error when split index exceeds 
/// the number of elements in the list.
/// </summary>
let splitAtSafe index xs = 
  if index < 0 
    then Error <| indexNegativeErr index
  elif index > List.length xs
    then Error <| indexTooLargeErr index (List.length xs)
  else
    Ok <| List.splitAt index xs

/// <summary>
/// Splits a list into two lists, at the given index.
/// Returns an IndexOutOfBounds Error when split index exceeds 
/// the number of elements in the list.
/// </summary>
let inline splitAt' index xs = splitAtSafe index xs 

/// <summary>
/// Splits the input list into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
/// </summary>
let splitIntoSafe count xs = 
  if count > 0 
  then Ok <| List.splitInto count xs
  else Error <| splitIntoErr count

/// <summary>
/// Splits the input list into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
/// </summary>
let inline splitInto' count xs = splitIntoSafe count xs

/// <summary>
/// Splits the input list into at most count chunks.
/// Same as <c>List.splitInto</c>, but restricts the input to a PositiveInt
/// </summary>
let splitIntoN (PositiveInt count) xs = List.splitInto count xs

/// <summary>
/// Returns a list that skips 1 element of the underlying list and then yields the
/// remaining elements of the list.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let tailSafe xs = 
  if List.isEmpty xs 
  then Error <| tailErr
  else Ok <| List.tail xs

/// <summary>
/// Returns a list that skips 1 element of the underlying list and then yields the
/// remaining elements of the list.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
/// </summary>
let inline tail' xs = tailSafe xs

/// <summary>
/// Returns the first N elements of the list.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let takeSafe count xs = 
  if List.length xs >= count && count >= 0
  then Ok <| List.take count xs
  else Error <| lazyTakeErr count

/// <summary>
/// Returns the first N elements of the list.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
/// </summary>
let inline take' count xs = takeSafe count xs

/// <summary>
/// Returns a list that yields sliding windows containing elements drawn from the input
/// list. Each window is returned as a fresh list.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let windowedSafe size xs = 
  if size > 0 
  then Ok <| List.windowed size xs
  else Error <| windowedErr size

/// <summary>
/// Returns a list that yields sliding windows containing elements drawn from the input
/// list. Each window is returned as a fresh list.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
/// </summary>
let inline windowed' size xs = windowedSafe size xs

/// <summary>
/// Returns a list that yields sliding windows containing elements drawn from the input
/// list. Each window is returned as a fresh list.
/// Same as <c>List.windowed</c>, but restricts the input to a PositiveInt
/// </summary>
let window (PositiveInt size) xs = List.windowed size xs

/// <summary>
/// Combines the two lists into a list of pairs. The two lists must have equal lengths.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let zipSafe xs ys = 
  if List.length xs = List.length ys
  then Ok <| List.zip xs ys 
  else Error <| zipErr (List.length xs) (List.length ys)

/// <summary>
/// Combines the two lists into a list of pairs. The two lists must have equal lengths.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline zip' xs ys = zipSafe xs ys

/// <summary>
/// Combines the three lists into a list of triples. The lists must have equal lengths.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let zip3Safe xs ys zs = 
  if List.length xs = List.length ys && List.length xs = List.length zs
  then Ok <| List.zip3 xs ys zs
  else Error <| zip3Err (List.length xs) (List.length ys) (List.length zs)

/// <summary>
/// Combines the three lists into a list of triples. The lists must have equal lengths.
/// Returns a DifferingLengths Error if the input lists have a different number of elements.
/// </summary>
let inline zip3' xs ys zs = zip3Safe xs ys zs

/// <summary>
/// Returns the transpose of the given sequence of lists.  Returns a DifferingLengths Error if
/// the input lists differ in length. 
/// </summary>
let transposeSafe xs = 
  match xs with 
  | SeqOneOrMore (head, tail) -> 
    let headLength = List.length head
    if Seq.forall (List.length >> (=) headLength) tail
    then Ok <| List.transpose xs
    else Error transposeErr
  | _ -> Ok []

/// <summary>
/// Returns the transpose of the given sequence of lists.  Returns a DifferingLengths Error if
/// the input lists differ in length. 
/// </summary>
let inline transpose' xs = transposeSafe xs

/// <summary>
/// Functions for manipulating NonEmpty Lists
/// </summary>
module NonEmpty =

  /// <summary>
  /// Creates a new NonEmptySeq with the provided head and tail.  
  /// </summary>
  let create head tail : NonEmptyList<_> = NonEmpty (head :: tail)

  /// <summary>
  /// Returns a NonEmpty array that contains one item only.
  /// </summary>
  let singleton x : NonEmptyList<_> = NonEmpty [x]
 
  /// <summary>
  /// Returns the average of the elements in the list.
  /// The elements are averaged using the <c>+</c> operator, 
  /// <c>DivideByInt</c> method and <c>Zero</c> property associated with the element type.
  /// </summary>
  let inline average (NonEmpty xs : NonEmptyList<_>) = List.average xs

  /// <summary>
  /// Returns the average of the results generated by applying the function to each element of the list.
  /// The elements are averaged using the <c>+</c> operator, <c>DivideByInt</c> method and <c>Zero</c> property associated with the generated type.
  /// </summary>
  let inline averageBy projection (NonEmpty xs : NonEmptyList<_>) = List.averageBy projection xs

  /// <summary>
  /// Returns the first element of the list.
  /// </summary>
  let head (NonEmpty xs : NonEmptyList<_>) = List.head xs
  
  /// <summary>
  /// Returns the last element of the list.
  /// </summary>
  let last (NonEmpty xs : NonEmptyList<_>) = List.last xs

  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.
  /// </summary
  let min (NonEmpty xs : NonEmptyList<_>) = List.min xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c>.
  /// </summary
  let max (NonEmpty xs : NonEmptyList<_>) = List.max xs

  /// <summary>
  /// Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c> on the function result.
  /// </summary>
  let minBy projection (NonEmpty xs : NonEmptyList<_>) = List.minBy projection xs

  /// <summary>
  /// Returns the greatest of all elements of the sequence, compared via <c>Operators.max</c> on the function result.
  /// </summary>
  let maxBy projection (NonEmpty xs : NonEmptyList<_>) = List.maxBy projection xs

  /// <summary>
  /// Returns the sequence after removing the first element.
  /// </summary>
  let tail (NonEmpty xs : NonEmptyList<_>) = List.tail xs
  
  /// <summary>
  /// Returns the tuple of the sequence's head and tail
  /// </summary>
  let uncons xs = (head xs, tail xs)
  
  /// <summary>
  /// Applies a function to each element of the sequence, threading an accumulator argument
  /// through the computation. Begin by applying the function to the first two elements.
  /// Then feed this result into the function along with the third element and so on.
  /// Return the final result.
  /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (f (...(f i0 i1)) iN-1) iN</c>.
  /// </summary>
  let reduce f (NonEmpty xs : NonEmptyList<_>) = List.reduce f xs

  /// <summary>
  /// Applies a function to each element of the list, starting from the end, threading an accumulator argument through the computation. 
  /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f i0 (...(f iN-1 iN))</c>.
  /// </summary>
  let reduceBack f (NonEmpty xs : NonEmptyList<_>) = List.reduceBack f xs

  /// <summary>
  /// O(n). Returns the length of the sequence.
  /// </summary>
  let length (NonEmpty xs : NonEmptyList<_>) = List.length xs
  
  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument
  /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
  /// then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let fold f initialState (NonEmpty xs : NonEmptyList<_>) = List.fold f initialState xs

  /// <summary>
  /// Builds a new collection whose elements are the corresponding elements of the input collection paired with the integer index (from 0) of each element.
  /// </summary>
  let indexed (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> = NonEmpty (List.indexed xs)

  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The given function will be applied
  /// as elements are demanded using the MoveNext method on enumerators retrieved from the
  /// object.
  /// </summary>
  let map f (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> = NonEmpty (List.map f xs)
  
  /// <summary>
  /// Builds a new collection whose elements are the results of applying the given function
  /// to each of the elements of the collection. The integer index passed to the
  /// function indicates the index (from 0) of element being transformed.
  /// </summary>
  let mapi f (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> =
    NonEmpty (List.mapi f xs)
  
  let private (<!>) f x = Result.map f x

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2Safe f (NonEmpty xs : NonEmptyList<_>) (NonEmpty ys : NonEmptyList<_>) : Result<NonEmptyList<_>,_> =
    NonEmpty <!> map2' f xs ys

  /// <summary>
  /// Build a new collection whose elements are the results of applying the given function
  /// to the corresponding elements of the two collections pairwise.  The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other sequence are ignored.  
  /// </summary>
  let map2' f (NonEmpty xs : NonEmptyList<_>) (NonEmpty ys : NonEmptyList<_>) : Result<NonEmptyList<_>,_> = 
    NonEmpty <!> map2' f xs ys

  /// <summary>
  /// Returns a new collection containing only the elements of the collection
  /// for which the given predicate returns "true". This is a synonym for Seq.where.
  /// </summary>
  let filter f (NonEmpty xs : NonEmptyList<_>) = List.filter f xs

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let append (NonEmpty xs : NonEmptyList<_>) (NonEmpty ys : NonEmptyList<_>) : NonEmptyList<_> = 
    NonEmpty (List.append xs ys)

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let appendL (NonEmpty xs : NonEmptyList<_>) ys : NonEmptyList<_> = 
    NonEmpty (List.append xs ys)

  /// <summary>
  /// Wraps the two given enumerations as a single concatenated enumeration.
  /// </summary>
  let appendR xs (NonEmpty ys : NonEmptyList<_>) : NonEmptyList<_> = 
    NonEmpty (List.append xs ys)

  /// <summary>
  /// Combines the given enumeration-of-enumerations as a single concatenated enumeration.
  /// </summary>
  let concat (NonEmpty xs : NonEmptyList<NonEmptyList<'a>>) : NonEmptyList<_> = 
    NonEmpty (xs |> List.map (fun (NonEmpty x) -> x) |> List.concat)

  /// <summary>
  /// Applies the given function to each element of the sequence and concatenates all the results.
  /// </summary>
  let collect (f : 'a -> NonEmptyList<'b>) (NonEmpty xs : NonEmptyList<'a>) : NonEmptyList<'b> = 
    let g = f >> (|NonEmpty|)
    NonEmpty (List.collect g xs)

  /// <summary>
  /// O(n), where n is count. Return the list which will remove at most 'n' elements of
  /// the input list.
  /// </summary>
  let drop (NaturalInt n) (NonEmpty xs : NonEmptyList<_>) = 
    if n >= List.length xs then []
    else xs.[n .. List.length xs - 1]

  /// <summary>
  /// O(n), where n is count. Return the list which will remove at most 'n' elements of
  /// the input list.
  /// CAUTION: This function will THROW for negative values of 'n'.
  /// </summary>
  let dropUnsafe n (NonEmpty xs : NonEmptyList<_>) = 
    match n with
    | Natural i ->
      if n >= List.length xs then []
      else xs.[n .. List.length xs - 1]
    | neg -> invalidArg "n" "Can't drop a negative number of values"

  /// <summary>
  /// O(n), where n is count. Return the list which will remove at most 'n' elements of
  /// the input list.
  /// This function will return the input list unaltered for negative values of 'n'.
  /// </summary>
  let dropLenient n (NonEmpty xs as lst : NonEmptyList<_>) = 
    match n with
    | Natural i -> drop i lst
    | neg -> xs


  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofListSafe (xs:_ list) : Result<NonEmptyList<_>,_> = 
    match xs with
    | Empty -> Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    | NotEmpty ys -> Ok <| ys

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmpty FSeq.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofList' xs = ofListSafe xs

  /// <summary>
  /// Returns a sequence of each element in the input sequence and its predecessor, with the
  /// exception of the first element which is only returned as the predecessor of the second element.
  /// </summary>
  let pairwise (NonEmpty xs : NonEmptyList<_>) = List.pairwise xs

  /// <summary>
  /// Returns a new sequence with the elements in reverse order.
  /// </summary>
  let rev (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> = NonEmpty (List.rev xs)

  /// <summary>
  /// Like fold, but computes on-demand and returns the sequence of intermediary and final results.
  /// </summary>
  let scan f initialState (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> = NonEmpty (List.scan f initialState xs)

  /// <summary>
  /// Builds an array from the given collection.
  /// </summary>
  let toArray (NonEmpty xs : NonEmptyList<_>) = List.toArray xs

  /// <summary>
  /// Builds a NonEmpty array from the given collection.
  /// </summary>
  let toNonEmptyArray xs : NonEmptyArray<_>= NonEmpty <| toArray xs

  /// <summary>
  /// Builds a List from the given collection.
  /// </summary>
  let toList (NonEmpty xs : NonEmptyList<_>) = xs

  /// <summary>
  /// Views the given NonEmptySeq as a sequence.
  /// </summary>
  let toSeq (NonEmpty xs : NonEmptyList<_>) : _ seq = upcast xs 

  /// <summary>
  /// Views the given NonEmpty List as a NonEmpty Seq.
  /// </summary>
  let toNonEmptySeq xs : NonEmptySeq<_> = NonEmpty <| toSeq xs

  /// <summary>
  /// Views the given NonEmpty List as a NonEmpty FSeq.
  /// </summary>
  let toNonEmptyFSeq (xs : NonEmptyList<_>) : NonEmptyFSeq<_> = NonEmpty <| fseq xs

  /// <summary>
  /// Returns the transpose of the given sequence of lists. Returns a DifferingLengths Error if
  /// the input lists differ in length. 
  /// </summary>
  let transposeSafe (xs : NonEmptySeq<NonEmptyList<'a>>) : Result< NonEmptyList<NonEmptyList<'a>> , DifferingLengths> = 
    let headLength = length (Seq.NonEmpty.head xs)
    if Seq.forall (length >> (=) headLength) (Seq.NonEmpty.tail xs)
    then Ok (List.transpose (Seq.map (|NonEmpty|) xs) |> List.map NonEmpty |> NonEmpty)
    else Error transposeErr

  /// <summary>
  /// Returns the transpose of the given sequence of lists. Returns a DifferingLengths Error if
  /// the input lists differ in length. 
  /// </summary>
  let inline transpose' xs = transposeSafe xs

  /// <summary>
  /// Returns a NonEmpty Array that when enumerated returns at most n elements.
  /// </summary>
  let truncate (PositiveInt n) (NonEmpty xs : NonEmptyList<_>) : NonEmptyList<_> = 
    NonEmpty (List.truncate n xs)

  /// <summary>
  /// Returns the first element for which the given function returns True.
  /// Return None if no such element exists.
  /// </summary>
  let tryFind predicate (NonEmpty xs : NonEmptyList<_>) = List.tryFind predicate xs

  /// <summary>
  /// O(n), where n is count. Return option the list which skips the first 'n' elements of
  /// the input list.
  /// </summary>
  let trySkip n (NonEmpty xs : NonEmptyList<_>) = Result.toOption <| skip' n xs 

  /// <summary>
  /// O(n), where n is count. Return the list which on consumption will consist of exactly 'n' elements of
  /// the input list.
  /// </summary>
  let tryTake n (NonEmpty xs : NonEmptyList<_>) = Result.toOption <| take' n xs

  /// <summary>
  /// Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
  /// when one sequence is exhausted any remaining elements in the other
  /// sequence are ignored.
  /// </summary>
  let zip' (NonEmpty xs : NonEmptyList<_>) (NonEmpty ys : NonEmptyList<_>) : Result<NonEmptyList<_>,_> = 
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
    |> Seq.NonEmpty.map Seq.NonEmpty.toNonEmptyList
    |> Seq.NonEmpty.toNonEmptyList

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
    |> FSeq.NonEmpty.map FSeq.NonEmpty.toNonEmptyList
    |> FSeq.NonEmpty.toNonEmptyList
