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