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
