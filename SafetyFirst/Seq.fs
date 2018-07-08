module SafetyFirst.Seq

open ResultDotNet.FSharp

open SafetyFirst.ErrorTypes

/// Returns the average of the elements in the sequence.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
let inline averageSafe source = 
  if Seq.isEmpty source
  then Error <| avgErr ()
  else Ok <| Seq.average source

/// Returns the average of the elements in the sequence.
/// Returns a SeqIsEmpty error if <c>source</c> has no elements.
let inline average' source = averageSafe source 

/// Returns the average of the results generated by applying the function to each element
/// of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
let inline averageBySafe selector xs = 
  if Seq.isEmpty xs
  then Error <| avgErr ()
  else Ok <| Seq.averageBy selector xs

/// Returns the average of the results generated by applying the function to each element
/// of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
let inline averageBy' selector xs = averageBySafe selector xs

/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
let chunkBySizeSafe size xs =
  if size <= 0 
  then Error chunkErr
  else Ok <| Seq.chunkBySize size xs

/// Divides the input sequence into chunks of size at most <c>size</c>.
/// Returns a NegativeInput Error if the <c>size</c> is less than zero.
let inline chunkBySize' size xs = chunkBySizeSafe size xs

/// If the input sequence has only one element, returns that element.
/// If the input sequence has more or less than one element, returns a WrongNumberOfElements Error.  
let exactlyOneSafe xs =
  let xs = Seq.cache (Seq.truncate 2 xs)
  match Seq.length xs with
  | 1 -> Ok <| Seq.head xs
  | 0 -> Error lessThanOneErr
  | _ -> Error moreThanOneErr 

/// If the input sequence has only one element, returns that element.
/// If the input sequence has more or less than one element, returns a WrongNumberOfElements Error.  
let inline exactlyOne' xs = exactlyOneSafe xs

/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
let findSafe predicate xs = 
  Seq.tryFind predicate xs 
  |> Result.ofOption findErr

/// Returns the first element for which the given function returns True.
/// Returns a NoMatchingElement Error if no such element is found.
let inline find' predicate xs = findSafe predicate xs

/// Returns the last element for which the given function returns True.
/// Return a NoMatchingElement Error if no such element exists.
let findBackSafe predicate xs = 
  Seq.tryFindBack predicate xs
  |> Result.ofOption findErr

/// Returns the last element for which the given function returns True.
/// Return a NoMatchingElement Error if no such element exists.
let inline findBack' predicate xs = findBackSafe predicate xs

/// Returns the index of the first element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
let findIndexSafe predicate xs = 
  Seq.tryFindIndex predicate xs
  |> Result.ofOption findErr

/// Returns the index of the first element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
let inline findIndex' predicate xs = findIndexSafe predicate xs

/// Returns the index of the last element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
let findIndexBackSafe predicate xs = 
  Seq.tryFindIndexBack predicate xs
  |> Result.ofOption findErr

/// Returns the index of the last element in the sequence
/// that satisfies the given predicate.
/// Return a NoMatchingElement Error if no such element exists.
let inline findIndexBack' predicate xs = findIndexBackSafe predicate xs

/// Returns the first element of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
let headSafe xs =
  if Seq.isEmpty xs 
  then Error headErr
  else Ok <| Seq.head xs

/// Returns the first element of the sequence.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
let inline head' xs = headSafe xs

/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
let itemSafe index xs = 
  let xs = ResizeArray (Seq.truncate (index + 1) xs)
  if index >= 0
  then 
    if Seq.length xs > index 
    then Ok <| xs.[index]
    else Error <| lazyIndexTooLargeErr index
  else Error <| indexNegativeErr index

/// Computes the element at the specified index in the collection.
/// Returns an IndexOutOfRange Error if the index is negative or exceeds the size of the collection.
let inline item' index xs = itemSafe index xs

/// Returns the last element of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let lastSafe xs = Seq.tryLast xs |> Result.ofOption lastErr  

/// Returns the last element of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline last' xs = lastSafe xs

/// Returns the greatest of all elements of the sequence, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let maxSafe<'a when 'a : comparison> (xs:'a seq) =
  if Seq.isEmpty xs 
  then Error <| maxErr
  else Ok <| Seq.max xs

/// Returns the greatest of all elements of the sequence, compared via Operators.max.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline max'<'a when 'a : comparison> (xs:'a seq) = maxSafe xs

/// Returns the greatest of all elements of the sequence, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let maxBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a seq) =
  if Seq.isEmpty xs
  then Error <| maxErr
  else Ok <| Seq.maxBy projection xs

/// Returns the greatest of all elements of the sequence, compared via Operators.max on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline maxBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a seq) = maxBySafe projection xs

/// Returns the lowest of all elements of the sequence, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let minSafe<'a when 'a : comparison> (xs:'a seq) =
  if Seq.isEmpty xs 
  then Error <| minErr
  else Ok <| Seq.min xs

/// Returns the lowest of all elements of the sequence, compared via Operators.min.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline min'<'a when 'a : comparison> (xs:'a seq) = minSafe xs

/// Returns the lowest of all elements of the sequence, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let minBySafe<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a seq) =
  if Seq.isEmpty xs 
  then Error <| minErr
  else Ok <| Seq.minBy projection xs

/// Returns the lowest of all elements of the sequence, compared via Operators.min on the function result.
/// Returns a SeqIsEmpty Error if <c>xs</c> has no elements.
let inline minBy'<'a, 'b when 'b : comparison> (projection:'a -> 'b) (xs:'a seq) = minBySafe projection xs

/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
let pickSafe chooser xs = 
  Seq.tryPick chooser xs
  |> Result.ofOption pickErr

/// Applies the given function to successive elements, returning the first
/// result where the function returns "Some(x)".
/// Return a NoMatchingElement Error if no such element exists.
let inline pick' chooser xs = pickSafe chooser xs

/// Applies a function to each element of the sequence, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// Returns a SeqIsEmpty Error if the sequence is empty.
let reduceSafe reduction xs = 
  if Seq.isEmpty xs
  then Error <| reduceErr
  else Ok <| Seq.reduce reduction xs

/// Applies a function to each element of the sequence, threading an accumulator argument
/// through the computation. Begin by applying the function to the first two elements.
/// Then feed this result into the function along with the third element and so on.
/// Return the final result.
/// Returns a SeqIsEmpty Error if the sequence is empty.
let inline reduce' reduction xs = reduceSafe reduction xs

/// Applies a function to each element of the sequence, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the sequence is empty.
let reduceBackSafe reduction xs = 
  if Seq.isEmpty xs
  then Error <| reduceErr
  else Ok <| Seq.reduceBack reduction xs

/// Applies a function to each element of the sequence, starting from the end, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
/// then computes <c>f i0 (...(f iN-1 iN))</c>.
/// Returns a SeqIsEmpty Error if the sequence is empty.
let inline reduceBack' reduction xs = reduceBackSafe reduction xs 

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
let skipSafe count (xs:_ seq) =
  if (Seq.length (Seq.truncate count xs) = count) || count < 0
  then Ok <| Seq.skip count xs
  else Error <| lazySkipErr count 

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
let inline skip' count xs = skipSafe count xs

/// Splits the input sequence into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
let splitIntoSafe count xs = 
  if count > 0 
  then Ok <| Seq.splitInto count xs
  else Error <| splitIntoErr count

/// Splits the input sequence into at most count chunks.
/// Returns a NegativeInput Error if <c>count</c> is not positive.
let inline splitInto' count xs = splitIntoSafe count xs

/// Returns a sequence that skips 1 element of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
let tailSafe xs = 
  if Seq.isEmpty xs 
  then Error <| tailErr
  else Ok <| Seq.tail xs

/// Returns a sequence that skips 1 element of the underlying sequence and then yields the
/// remaining elements of the sequence.
/// Returns a SeqIsEmpty Error if <c>xs</c> contains no elements.
let inline tail' xs = tailSafe xs

/// Returns the first N elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
let takeSafe count xs = 
  let xs = ResizeArray (Seq.truncate count xs)
  if Seq.length xs = count 
  then Ok <| seq xs
  else Error <| lazyTakeErr count

/// Returns the first N elements of the sequence.
/// Returns a NotEnoughElements Error if <c>count</c> exceeds the length of <c>xs</c> 
let inline take' count xs = takeSafe count xs

/// Returns a sequence that yields sliding windows containing elements drawn from the input
/// sequence. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
let windowedSafe size xs = 
  if size > 0 
  then Ok <| Seq.windowed size xs
  else Error <| windowedErr size

/// Returns a sequence that yields sliding windows containing elements drawn from the input
/// sequence. Each window is returned as a fresh array.
/// Returns a NegativeInput Error when <c>size</c> is not positive.
let inline windowed' size xs = windowedSafe size xs
