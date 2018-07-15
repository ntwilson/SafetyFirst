namespace SafetyFirst

module FiniteSeq = 

  /// <summary>
  /// Splits a sequence at every occurrence of an element satisfying <c>isSplitElement</c>.
  /// The split occurs immediately before each element that satisfies <c>isSplitElement</c>,
  /// and the element satisfying <c>isSplitElement</c> will be included as the first element of 
  /// the sequence following the split.
  /// This function returns a tuple of the head element (the values leading up to the first split),
  /// and the tail (all of the sequences after the first split).  This is because the head element
  /// may or may not be empty, while all of the sequences after the first split are guaranteed
  /// to be non-empty.  Also, you are guaranteed to have a head element, but the tail may or may
  /// not be empty.
  /// For example:
  /// <code>
  /// FiniteSeq.split ((=) 100) (fseq [1;2;3;100;100;4;100;5;6])
  ///   //returns ([1;2;3], [[100];[100;4];[100;5;6]])
  /// </code>
  /// </summary>
  let split isSplitElement xs = 
    let notSplitElement = not << isSplitElement
    let singleSplit input = (FSeq.takeWhile notSplitElement input, FSeq.skipWhile notSplitElement input)

    let rec split' input =
      fseq (
        seq { 
          match input with
          | NotEmpty remainder ->
            let head, tail = NonEmptySeq.uncons remainder
            let contiguous, restOfInput = singleSplit tail
            let nextChunk = NonEmptySeq.create head contiguous
            yield nextChunk
            yield! split' restOfInput
          | Empty -> ()
        })

    let upToFirstSplit, remainder = singleSplit xs
    in (upToFirstSplit, split' remainder)

module FSeq = 
  /// <summary>
  /// Splits a sequence at every occurrence of an element satisfying <c>isSplitElement</c>.
  /// The split occurs immediately before each element that satisfies <c>isSplitElement</c>,
  /// and the element satisfying <c>isSplitElement</c> will be included as the first element of 
  /// the sequence following the split.
  /// This function returns a tuple of the head element (the values leading up to the first split),
  /// and the tail (all of the sequences after the first split).  This is because the head element
  /// may or may not be empty, while all of the sequences after the first split are guaranteed
  /// to be non-empty.  Also, you are guaranteed to have a head element, but the tail may or may
  /// not be empty.
  /// For example:
  /// <code>
  /// FSeq.split ((=) 100) (fseq [1;2;3;100;100;4;100;5;6])
  ///   //returns ([1;2;3], [[100];[100;4];[100;5;6]])
  /// </code>
  /// </summary>
  let split isSplitElement xs = FiniteSeq.split isSplitElement xs

module NonEmptySeq = 
  let private uncurry f (a, b) = f a b

  /// <summary>
  /// Splits a sequence between each pair of adjacent elements that satisfy <c>splitBetween</c>.
  /// For example:
  /// <code>
  /// NonEmptySeq.splitPairwise (=) (NonEmptySeq.create 0 (fseq [1;1;2;3;4;4;4;5]))
  ///   //returns [[0;1];[1;2;3;4];[4];[4;5]]
  /// </code>
  /// </summary>
  let splitPairwise splitBetween xs =
    let (headGroup, tailGroups) = 
      NonEmptySeq.pairwise xs
      |> FSeq.split (uncurry splitBetween) 
    let firstGroup = NonEmptySeq.create (NonEmptySeq.head xs) (FSeq.map snd headGroup)
    in NonEmptySeq.create firstGroup (FSeq.map (NonEmptySeq.map snd) tailGroups)

