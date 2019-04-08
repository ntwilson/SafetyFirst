module SafetyFirst.String

open System

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

