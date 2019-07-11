module SafetyFirst.String

open System
open SafetyFirst.Numbers

/// <summary>
/// Alternate version of <c>String.init</c> with specific, safe types.
/// Creates a new string whose characters are the results of applying a specified function to each index and concatenating the resulting strings.
/// </summary>
let initN (count:NaturalInt) initializer = String.init count.Value (initializer << (NaturalInt.verify >> Option.unless "F# core assumption failed: String.init called an initializer with a negative index."))

/// <summary>
/// Alternate version of <c>String.replicate</c> with specific, safe types.
/// Generates a new sequence which, when iterated, will return the given value for every element, up to the given count.
/// </summary>
let replicateN (count:NaturalInt) initial = String.replicate count.Value initial

/// <summary>
/// Splits a string into substrings based on the strings in a sequence.
/// This is equivalent of the regular Split method on strings, viewed as
/// a NonEmpty Array.
/// </summary>
let split (delimiter:string seq) (text:string) : NonEmptyArray<_> = 
  NonEmpty <| text.Split(Seq.toArray delimiter, StringSplitOptions.None)

/// <summary>
/// Splits a string into substrings based on the strings in a sequence, excluding any empty array elements.
/// This is equivalent of the regular Split method on strings.
/// </summary>
let inline splitNoEmptyEntries (delimiter:string seq) (text:string) =
  text.Split(Seq.toArray delimiter, StringSplitOptions.RemoveEmptyEntries)

