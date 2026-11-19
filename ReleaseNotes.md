# Version 5.5.0

## New features:

- The `result`, `option`, and `voption` computation expressions now support `if ... then` without an `else` (e.g., `if verbose then printfn "..."`, or `if x < 0 then do! Error "negative"` to return early) ([#97](https://github.com/ntwilson/SafetyFirst/issues/97))

- Adds a nested `Result` module to the List/Array/Set/Map/Dictionary modules and to their `NonEmpty` modules (e.g., `List.Result`, `Map.NonEmpty.Result`) ([#106](https://github.com/ntwilson/SafetyFirst/issues/106)), each with:

  - `traverse`: maps with a Result-producing function, returning Ok of the mapped collection, or the first Error without evaluating the rest
  - `sequence`: same as `traverse`, but for a collection that already holds Results
  - `collect`: returns Ok of all the values, or an Error of every error as a non-empty collection (e.g., `NonEmptyList<'e>`, or a `NonEmptyMap<'k, 'e>` keyed like the input)
  - `partition`: splits a collection of Results into the Ok values and the Error values
  - `partitionMap`: maps with a Result-producing function and splits the results into the Ok values and the Error values

  The Map and Dictionary versions pass the key to the function, e.g. `Map.Result.traverse : ('k -> 'a -> Result<'b, 'e>) -> Map<'k, 'a> -> Result<Map<'k, 'b>, 'e>`.

  The Set versions omit `partition` and `partitionMap` because it's not decided yet what collection should contain the errors.

- Brings the `NonEmpty` submodules closer to parity with their parent modules, so a NonEmpty collection no longer needs converting back before calling these ([#103](https://github.com/ntwilson/SafetyFirst/issues/103)). Where the input being non-empty guarantees a non-empty result, the result is NonEmpty.

### Array.NonEmpty and List.NonEmpty

- `chunkBySize` (Safe/'/try), `chunksOf`
- `exactlyOne` (Safe/'/try), `item` (Safe/'), `pick` (Safe/'), `tryPick`
- `fold2`, `foldBack2`, `forall2`, `iter2`, `iteri2` (Safe/'/try)
- `mapi2`, `map3` (Safe/'/try)
- `skipUntilIncluding`, `splitAt` (Safe/'/try), `splitInto` (Safe/'/try), `splitIntoN`
- `sub` (Safe/'/try) - Array only
- `sum`, `sumBy`, `iter`, `iteri`, `choose`, `takeWhile`, `skipWhile`, `sortWith`, `scanBack`, `mapFold`, `mapFoldBack`

### Seq.NonEmpty

- `averageBy`, `last`, `reduceBack`, `foldBack`, `contains`, `countBy`, `exists`, `forall`, `groupBy`
- `chunkBySize` (Safe/'/try), `chunksOf`, `splitInto` (Safe/'/try), `splitIntoN`, `windowed` (Safe/'/try), `window`
- `exactlyOne` (Safe/'/try), `item` (Safe/'), `pick` (Safe/'), `tryPick`
- `find`, `findBack`, `findIndex`, `findIndexBack` (Safe/'), `tryFindBack`, `tryFindIndex`, `tryFindIndexBack`
- `drop`, `dropLenient`, `skipUntilIncluding`
- `sort`, `sortBy`, `sortDescending`, `sortByDescending`, `zip3`
- `sum`, `sumBy`, `iter`, `iteri`, `choose`, `takeWhile`, `skipWhile`, `sortWith`, `scanBack`, `mapFold`, `mapFoldBack`

### FSeq.NonEmpty

- `cons`, `chunkBySize` (Safe/'/try), `chunksOf`, `splitInto` (Safe/'/try), `splitIntoN`, `skipUntilIncluding`
- `sum`, `sumBy`, `iter`, `iteri`, `choose`, `takeWhile`, `skipWhile`, `sortWith`, `scanBack`, `mapFold`, `mapFoldBack`

Likewise adds nested `Option` and `ValueOption` modules (e.g., `Array.Option`, `Map.NonEmpty.ValueOption`), each with `traverse` and `sequence`. Unlike the existing `Option.traverse` and `Option.sequence`, which always return a list, these return the same collection type as the input. ([#113](https://github.com/ntwilson/SafetyFirst/issues/113))

## Bug fixes:

- `Dictionary.map` now keeps the input's key comparer when the input is a `Dictionary` (e.g. a case-insensitive dictionary stays case-insensitive). Other `IDictionary` implementations don't expose their comparer, so they still get the default one.

# Version 5.4.2

## Bug fixes:

- Fix an inadvertently changed signature breaking binary compatibility with 5.3.0

# Version 5.4.0

## New features:

Adds new functions for the List/Array/Seq/FSeq modules:

- `splitPairwise`
- `split`
- `takeUntilIncluding`
- `skipUntilIncluding`

### InfiniteSeq

InfiniteSeq has been reworked. It is now iterable as a regular sequence. When dealing with infinite sequences, a hang should not be considered a recoverable error with programmatic mitigation (other than possibly with a global exception handler), rather it should be considered a bug needing a fix. Therefore, InfiniteSeq is no longer designed to return a Result in the event of a hang - it's meant to throw an exception instead. Functions like `InfiniteSeq.item` now either crash for a hang or return the item without Result. Existing Result-returning functions like `item'` or Option-returning functions like `tryItem` still exist but are marked deprecated, and will be removed in version 6.0. If you still need the functionality to programmatically recover from a hang, then switch to a `try ... with :? InfiniteSequenceEvaluationHung ->` block.

New functions include:

- `initBounded`: Same as `init` but without the need for the `MaxElements` union case
- `initUnbounded`: Create an "unsafe" InfiniteSeq that can hang if misused
- `isHungAfter`: apply a new upper bound to any InfiniteSeq
- `assume`: assume an existing seq is infinite
- `append`: prepend any seq to the front of an InfiniteSeq
- `item`: same as `Seq.item`, but safe for infinite sequences (barring a hang)
- `take`: same as `Seq.take`, but safe for infinite sequences (barring a hang)
- `takeWhile`: same as `Seq.takeWhile`, but safe for infinite sequences (barring a hang)
- `head`: same as `Seq.head`, but safe for infinite sequences (barring a hang)
- `uncons`: same as `Seq.uncons`, but safe for infinite sequences (barring a hang)
- `find`: same as `Seq.find`, but safe for infinite sequences (barring a hang)
- `skipUntilIncluding`: same as `Seq.skipUntilIncluding`
- `splitPairwise`: same as `Seq.splitPairwise`
- `split`: same as `Seq.split`

Also `Seq.isHungAfter` exists to take a potentially infinite seq that _isn't_ defined as an `InfiniteSeq` and apply an upper bound to consider the sequence hung if it produces more elements than some max number.

## Deprecations:

Existing Result-returning functions like `item'` or Option-returning functions like `tryItem` in the InfiniteSeq module are marked deprecated, and will be removed in version 6.0. If you still need the functionality to programmatically recover from a hang, then switch to a `try ... with :? InfiniteSequenceEvaluationHung ->` block. These include:

- `item'`
- `itemSafe`
- `tryItem`
- `take'`
- `takeSafe`
- `tryTake`
- `takeWhile'`
- `tryTakeWhile`
- `head'`
- `tryHead`
- `uncons'`
- `tryUncons`
- `find'`
- `tryFind`

# Version 5.3.0

## New features:

- Adds `zipper` computation expressions for all collection types that will zip any number of sequences together.
- Adds several zipping functions to the Map module.

# Version 5.2.0

## New features:

- Adds `try...with`, `try...finally`, `use`, and `use!` functionality to the `option` and `result` computation expressions. ([#89](https://github.com/ntwilson/SafetyFirst/issues/89))

# Version 5.1.0

## New features:

- Adds any missing `try...` functions to match any `Result` returning functions in all the collection modules.

# Versions 5.0.3 - 5.0.6

These releases just fix broken XML documentation in the nuget package and include no code changes.

# Version 5.0.2

This release just adds the README properly to the nuget package and includes no code changes.

# Version 5.0.1

## New features:

- Adds `minElement'` and `maxElement'` functions to the `Set` module.

# Version 5.0.0

This is a small release, but technically breaking because it adds some dependencies on other libraries.

## New features:

- Adds `sequence` and `traverse` capabilities to any `NonEmpty` collection. You could call the static members directly, but this is mostly valuable for users that are already using F#+.
- Extends the `result` computation expression to include an applicative instance. If you use `and!` in a `result` computation expression, it will add the errors together instead of halting on the first failure (uses F#+'s semigroup `++` to add the errors. Don't worry if you're not an F#+ user! It can append many common types such as strings, arrays, lists, etc. See the list of types that can be appended out-of-the-box [here](https://fsprojects.github.io/FSharpPlus/abstraction-semigroup.html#Concrete-implementations)).

## Internal changes:

- Removes the LazyList maintained in this copy and swaps it out for the FSharpx.Collections LazyList. (The implementation was initially copied because this library supported .net standard prior to FSharpx.Collections).

## Breaking changes:

- Adds a dependency on FSharpPlus >= 1.0.0
- Adds a dependency on FSharpx.Collections >= 2.1.3
