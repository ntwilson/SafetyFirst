module SafetyFirst.Set

open SafetyFirst.ErrorTypes

/// <summary>
/// Returns the lowest element in the set according to the ordering being used for the set.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// <summary>
let minElementSafe<'a when 'a : comparison> (xs:Set<'a>) = 
  if Set.isEmpty xs 
  then Error minErr
  else Ok <| Set.minElement xs

/// <summary>
/// Returns the lowest element in the set according to the ordering being used for the set.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// <summary>
let inline minElement'<'a when 'a : comparison> (xs:Set<'a>) = minElementSafe xs

/// <summary>
/// Returns the highest element in the set according to the ordering being used for the set.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// <summary>
let maxElementSafe<'a when 'a : comparison> (xs:Set<'a>) = 
  if Set.isEmpty xs 
  then Error maxErr
  else Ok <| Set.maxElement xs

/// <summary>
/// Returns the highest element in the set according to the ordering being used for the set.
/// Returns a SeqIsEmpty error if <c>xs</c> has no elements.
/// <summary>
let maxElement'<'a when 'a : comparison> (xs:Set<'a>) = maxElementSafe xs

module NonEmpty = 

  /// <summary>
  /// Creates a new NonEmptySet with the provided head and tail.  
  /// The tail is constrained to be finite.  If the tail is infinite,
  /// use Seq.NonEmpty.create instead.
  /// </summary>
  let create head tail : NonEmptySet<_> = NonEmpty (Set.add head (set tail))

  /// <summary>
  /// Returns a set that contains one item only.
  /// </summary>
  let singleton head : NonEmptySet<_> = NonEmpty (Set.singleton head)

  /// <summary>
  /// Returns the lowest element in the set according to the ordering being used for the set.
  /// </summary>
  let minElement (NonEmpty xs : NonEmptySet<_>) = Set.minElement xs

  /// <summary>
  /// Returns the highest element in the set according to the ordering being used for the set.
  /// </summary>
  let maxElement (NonEmpty xs : NonEmptySet<_>) = Set.maxElement xs

  /// <summary>
  /// Returns a new set with an element added to the set.
  /// If you're looking for an overload where the input set can be empty,
  /// use <c>Set.create</c> instead. 
  /// No exception is raised if the set already contains the given element.
  /// </summary>
  let add x (NonEmpty xs : NonEmptySet<_>) : NonEmptySet<_> = NonEmpty (Set.add x xs)

  /// <summary>
  /// Evaluates to "true" if the given element is in the given set.
  /// </summary>
  let contains x (NonEmpty xs : NonEmptySet<_>) = Set.contains x xs

  /// <summary>
  /// Returns the number of elements in the set. Same as size.
  /// </summary>
  let count (NonEmpty xs : NonEmptySet<_>) = Set.count xs

  /// <summary>
  /// Returns a new set with the elements of the second set removed from the first.
  /// </summary>
  let difference (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.difference xs ys

  /// <summary>
  /// Returns a new set with the elements of the second set removed from the first.
  /// </summary>
  let differenceL (NonEmpty xs : NonEmptySet<_>) ys = Set.difference xs ys

  /// <summary>
  /// Returns a new set with the elements of the second set removed from the first.
  /// </summary>
  let differenceR xs (NonEmpty ys : NonEmptySet<_>) = Set.difference xs ys

  /// <summary>
  /// Tests if any element of the collection satisfies the given predicate. 
  /// If the input function is <c>p</c> and the elements are <c>i0...iN</c> 
  /// then computes <c>p i0 or ... or p iN</c>.
  /// </summary>
  let exists predicate (NonEmpty xs : NonEmptySet<_>) = Set.exists predicate xs

  /// <summary>
  /// Returns a new collection containing only the elements of the collection 
  /// for which the given predicate returns True.
  /// </summary>
  let filter predicate (NonEmpty xs : NonEmptySet<_>) = Set.filter predicate xs

  /// <summary>
  /// Applies a function to each element of the collection, threading an accumulator argument through the computation. 
  /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (... (f s i0)...) iN</c>
  /// </summary>
  let fold folder initialState (NonEmpty xs : NonEmptySet<_>) = Set.fold folder initialState xs

  /// <summary>
  /// Applies a function to each element of the collection, starting from the end, threading an accumulator argument through the computation. 
  /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f i0 (... (f iN s)...)</c>
  /// </summary>
  let foldBack folder (NonEmpty xs : NonEmptySet<_>) initialState = Set.foldBack folder xs initialState 

  /// <summary>
  /// Tests if all elements of the collection satisfy the given predicate. 
  /// If the input function is <c>p</c> and the elements are <c>i0...iN</c> 
  /// then computes <c>p i0 &amp;&amp; ... &amp;&amp; p iN</c>.
  /// </summary>
  let forall predicate (NonEmpty xs : NonEmptySet<_>) = Set.forall predicate xs

  /// <summary>
  /// Computes the intersection of the two sets.
  /// </summary>
  let intersect (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.intersect xs ys

  /// <summary>
  /// Computes the intersection of the two sets.
  /// </summary>
  let intersectL (NonEmpty xs : NonEmptySet<_>) ys = Set.intersect xs ys

  /// <summary>
  /// Computes the intersection of the two sets.
  /// </summary>
  let intersectR xs (NonEmpty ys : NonEmptySet<_>) = Set.intersect xs ys

  /// <summary>
  /// Computes the intersection of a sequence of sets. The sequence must be non-empty.
  /// </summary>
  let intersectMany (NonEmpty xs : NonEmpty<#seq<NonEmptySet<_>>, _>) = 
    Set.intersectMany (Seq.map (|NonEmpty|) xs)

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the first set are in the second, and at least one element of the second is not in the first.
  /// </summary>
  let isProperSubset (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.isProperSubset xs ys 

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the first set are in the second, and at least one element of the second is not in the first.
  /// </summary>
  let isProperSubsetL (NonEmpty xs : NonEmptySet<_>) ys = Set.isProperSubset xs ys 

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the first set are in the second, and at least one element of the second is not in the first.
  /// </summary>
  let isProperSubsetR xs (NonEmpty ys : NonEmptySet<_>) = Set.isProperSubset xs ys 

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the second set are in the first, and at least one element of the first is not in the second.
  /// </summary>
  let isProperSuperset (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.isProperSuperset xs ys

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the second set are in the first, and at least one element of the first is not in the second.
  /// </summary>
  let isProperSupersetL (NonEmpty xs : NonEmptySet<_>) ys = Set.isProperSuperset xs ys

  /// <summary>
  /// Evaluates to <c>true</c> if all elements of the second set are in the first, and at least one element of the first is not in the second.
  /// </summary>
  let isProperSupersetR xs (NonEmpty ys : NonEmptySet<_>) = Set.isProperSuperset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the first set are in the second.
  /// </summary>
  let isSubset (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.isSubset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the first set are in the second.
  /// </summary>
  let isSubsetL (NonEmpty xs : NonEmptySet<_>) ys = Set.isSubset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the first set are in the second.
  /// </summary>
  let isSubsetR xs (NonEmpty ys : NonEmptySet<_>) = Set.isSubset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the second set are in the first.
  /// </summary>
  let isSuperset (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) = Set.isSuperset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the second set are in the first.
  /// </summary>
  let isSupersetL (NonEmpty xs : NonEmptySet<_>) ys = Set.isSuperset xs ys

  /// <summary>
  /// Evaluates to "true" if all elements of the second set are in the first.
  /// </summary>
  let isSupersetR xs (NonEmpty ys : NonEmptySet<_>) = Set.isSuperset xs ys

  /// <summary>
  /// Applies the given function to each element of the set, in order according to the comparison function.
  /// </summary>
  let iter f (NonEmpty xs : NonEmptySet<_>) = Set.iter f xs

  /// <summary>
  /// Returns a new collection containing the results of applying the 
  /// given function to each element of the input set.
  /// </summary>
  let map f (NonEmpty xs : NonEmptySet<_>) = NonEmpty (Set.map f xs)

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofArraySafe xs : Result<NonEmptySet<_>, _> = 
    if Array.isEmpty xs
    then Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    else Ok (NonEmpty (Set.ofArray xs))

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofArray' xs = ofArraySafe xs 

  /// <summary>
  /// Builds a NonEmptySet that contains the same elements as the given NonEmptyArray.
  /// </summary>
  let ofNonEmptyArray (NonEmpty xs : NonEmptyArray<_>) : NonEmptySet<_> = 
    NonEmpty (Set.ofArray xs)

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofSeqSafe xs : Result<NonEmptySet<_>, _> = 
    if Seq.isEmpty xs
    then Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    else Ok (NonEmpty (Set.ofSeq xs))

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofSeq' xs = ofSeqSafe xs 

  /// <summary>
  /// Builds a NonEmptySet that contains the same elements as the given NonEmptySeq.
  /// </summary>
  let ofNonEmptySeq (NonEmpty xs : NonEmptySeq<_>) : NonEmptySet<_> = 
    NonEmpty (Set.ofSeq xs)

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let ofListSafe xs : Result<NonEmptySet<_>, _> = 
    if List.isEmpty xs
    then Error <| SeqIsEmpty "Assertion that a sequence is not empty failed."
    else Ok (NonEmpty (Set.ofList xs))

  /// <summary>
  /// Asserts that <c>xs</c> is not empty, creating a NonEmptySet.
  /// Returns a SeqIsEmpty Error if <c>xs</c> is empty.
  /// </summary>
  let inline ofList' xs = ofListSafe xs 

  /// <summary>
  /// Builds a NonEmptySet that contains the same elements as the given NonEmptyList.
  /// </summary>
  let ofNonEmptyList (NonEmpty xs : NonEmptyList<_>) : NonEmptySet<_> = 
    NonEmpty (Set.ofList xs)

  /// <summary>
  /// Splits the set into two sets containing the elements for which the given predicate returns true and false respectively.
  /// </summary>
  let partition f (NonEmpty xs : NonEmptySet<_>) = Set.partition f xs

  /// <summary>
  /// Returns a new set with the given element removed. 
  /// No exception is raised if the set doesn't contain the given element.
  /// </summary>
  let remove element (NonEmpty xs : NonEmptySet<_>) = Set.remove element xs

  /// <summary>
  /// Builds an array that contains the elements of the set in order.
  /// </summary>
  let toArray (NonEmpty xs : NonEmptySet<_>) = Set.toArray xs

  /// <summary>
  /// Builds a NonEmptyArray that contains the elements of the set in order.
  /// </summary>
  let toNonEmptyArray (NonEmpty xs : NonEmptySet<_>) : NonEmptyArray<_> = NonEmpty (Set.toArray xs)

  /// <summary>
  /// Builds a list that contains the elements of the set in order.
  /// </summary>
  let toList (NonEmpty xs : NonEmptySet<_>) = Set.toList xs

  /// <summary>
  /// Builds a NonEmptyList that contains the elements of the set in order.
  /// </summary>
  let toNonEmptyList (NonEmpty xs : NonEmptySet<_>) : NonEmptyList<_> = NonEmpty (Set.toList xs)

  /// <summary>
  /// Builds a seq that contains the elements of the set in order.
  /// </summary>
  let toSeq (NonEmpty xs : NonEmptySet<_>) = Set.toSeq xs

  /// <summary>
  /// Builds a NonEmptySeq that contains the elements of the set in order.
  /// </summary>
  let toNonEmptySeq (NonEmpty xs : NonEmptySet<_>) : NonEmptySeq<_> = NonEmpty (upcast xs)

  /// <summary>
  /// Computes the union of the two sets.
  /// </summary>
  let union (NonEmpty xs : NonEmptySet<_>) (NonEmpty ys : NonEmptySet<_>) : NonEmptySet<_> = 
    NonEmpty (Set.union xs ys)

  /// <summary>
  /// Computes the union of the two sets.
  /// </summary>
  let unionL (NonEmpty xs : NonEmptySet<_>) ys : NonEmptySet<_> = 
    NonEmpty (Set.union xs ys)

  /// <summary>
  /// Computes the union of the two sets.
  /// </summary>
  let unionR xs (NonEmpty ys : NonEmptySet<_>) : NonEmptySet<_> = 
    NonEmpty (Set.union xs ys)

  let unionMany (NonEmpty xs : NonEmpty<#seq<NonEmptySet<_>>, _>) : NonEmptySet<_> = 
    NonEmpty (Set.unionMany (Seq.map (|NonEmpty|) xs))

  