namespace SafetyFirst.Numbers

/// <summary>
/// An integer >= 0
/// </summary>
[<Struct>]
type NaturalInt = private NaturalInt of int

[<Struct>]
type NegativeInt = private NegativeInt of int

/// <summary>
/// An integer > 0.  If you want to include 0, use <c>NaturalInt</c>
/// </summary>
[<Struct>]
type PositiveInt = private PositiveInt of int

[<AutoOpen>]
module NaturalIntMatchers = 
  let (|NaturalInt|) (NaturalInt i) = i
  let (|PositiveInt|) (PositiveInt i) = i
  let (|NegativeInt|) (NegativeInt i) = i
  let (|Nat|Neg|) i = 
    if i < 0 
    then Neg (NegativeInt i)
    else Nat (NaturalInt i)

  let (|Positive|Negative|Zero|) = function
    | i when i > 0 -> Positive (PositiveInt i)
    | i when i < 0 -> Negative (NegativeInt i)
    | _ -> Zero

module NaturalInt = 
  let test i = i >= 0
  let verify i = 
    if test i
    then Some (NaturalInt i)
    else None

  /// <summary>
  /// Asserts that the integer passed in is a natural int (>= 0).  Will THROW if given a negative value.
  /// This is intended to remove some of the overhead when the proof is obvious, such as
  /// <code>
  /// Array.drop (NaturalInt.assume 5) arr
  /// </code>
  /// so you don't need to do something like
  /// <code>
  /// match 5 with 
  /// | Natural x -> Array.drop x arr
  /// | _ -> ???
  /// </code>
  /// It is NOT recommended for use with calculated values, since calculations can be altered such that assumptions
  /// once correct are made invalid, and there would be no protection against such a mistake.  In this case, it's 
  /// recommended to use <c>NaturalInt.create</c> or the <c>(|Natural|Negative|)</c> pattern.
  /// </summary>
  let assume i = 
    if not <| test i then invalidArg "i" (sprintf "Assertion failed trying to create a NaturalInt: %i >= 0" i)
    else NaturalInt i

module NegativeInt =  
  let test i = i < 0
  let verify i = 
    if test i
    then Some (NegativeInt i) 
    else None  

  /// <summary>
  /// Asserts that the integer passed in is negative.  Will THROW if given a non-negative value.
  /// This is intended to remove some of the overhead when the proof is obvious, such as
  /// <code>
  /// let pos = PositiveInt.assume 5
  /// let neg = NegativeInt.assume (-1 * positiveInt)
  /// </code>
  /// so you don't need to do something like
  /// <code>
  /// let neg = 
  ///   match -1 * positiveInt with 
  ///   | Negative x -> x
  ///   | _ -> ???
  /// </code>
  /// It is NOT recommended for use with calculated values, since calculations can be altered such that assumptions
  /// once correct are made invalid, and there would be no protection against such a mistake.  In this case, it's 
  /// recommended to use <c>NegativeInt.create</c> or the <c>(|Natural|Negative|)</c> pattern.
  /// </summary>
  let assume i = 
    if not <| test i then invalidArg "i" (sprintf "Assertion failed trying to create a NegativeInt: %i < 0" i)
    else NegativeInt i

module PositiveInt = 
  let private test i = i > 0

  let verify i =
    if test i
    then Some (PositiveInt i)
    else None

  /// <summary>
  /// Asserts that the integer passed in is positive (> 0).  Will THROW if given a zero or negative value.
  /// This is intended to remove some of the overhead when the proof is obvious, such as
  /// <code>
  /// Array.chunkBySize (PositiveInt.assume 5) arr
  /// </code>
  /// so you don't need to do something like
  /// <code>
  /// match 5 with 
  /// | Positive x -> Array.chunkBySize x arr
  /// | _ -> ???
  /// </code>
  /// It is NOT recommended for use with calculated values, since calculations can be altered such that assumptions
  /// once correct are made invalid, and there would be no protection against such a mistake.  In this case, it's 
  /// recommended to use <c>PositiveInt.create</c> or the <c>(|Positive|Negative|Zero|)</c> pattern.
  /// </summary>
  let assume i = 
    if not <| test i then invalidArg "i" (sprintf "Assertion failed trying to create a PositiveInt: %i > 0" i)  
    else PositiveInt i