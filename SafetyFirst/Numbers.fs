namespace SafetyFirst.Numbers

/// <summary>
/// An integer >= 0
/// </summary>
[<Struct>]
type NaturalInt = 
  private | NaturalInt of int

  static member zero = NaturalInt 0

  member this.Value = let (NaturalInt t) = this in t
  static member value (t:NaturalInt) = t.Value
  static member op_Implicit (t:NaturalInt) = t.Value

  member this.Increment = let (NaturalInt t) = this in PositiveInt (t + 1)
  static member increment (t:NaturalInt) = t.Increment

and [<Struct>] NegativeInt = 
  private | NegativeInt of int

  member this.Value = let (NegativeInt n) = this in n
  static member value (n:NegativeInt) = n.Value
  static member op_Implicit (n:NegativeInt) = n.Value

  member this.Decrement = let (NegativeInt n) = this in NegativeInt (n - 1)
  static member decrement (n:NegativeInt) = n.Decrement

  member this.Opposite = let (NegativeInt n) = this in PositiveInt -n
  static member opposite (n:NegativeInt) = n.Opposite

/// <summary>
/// An integer > 0.  If you want to include 0, use <c>NaturalInt</c>
/// </summary>
and [<Struct>] PositiveInt = 
  private | PositiveInt of int

  member this.Value = let (PositiveInt p) = this in p
  static member value (p:PositiveInt) = p.Value
  static member op_Implicit (p:PositiveInt) = p.Value

  member this.AsNatural = let (PositiveInt p) = this in NaturalInt p
  static member asNatural (p:PositiveInt) = p.AsNatural

  member this.Increment = let (PositiveInt p) = this in PositiveInt (p + 1)
  static member increment (p:PositiveInt) = p.Increment

  member this.Decrement = let (PositiveInt p) = this in NaturalInt (p - 1)
  static member decrement (p:PositiveInt) = p.Decrement

  member this.Opposite = let (PositiveInt p) = this in NegativeInt -p
  static member opposite (p:PositiveInt) = p.Opposite

[<AutoOpen>]
module NaturalIntMatchers = 
  let (|NaturalInt|) (NaturalInt i) = i
  let (|PositiveInt|) (PositiveInt i) = i
  let (|NegativeInt|) (NegativeInt i) = i
  let (|Natural|NonNatural|) i = 
    if i < 0 
    then NonNatural (NegativeInt i)
    else Natural (NaturalInt i)

  let (|Positive|Negative|Zero|) = function
    | i when i > 0 -> Positive (PositiveInt i)
    | i when i < 0 -> Negative (NegativeInt i)
    | _ -> Zero

  let (|PositiveNatural|ZeroNatural|) = function
    | (NaturalInt i) when i > 0 -> PositiveNatural (PositiveInt i)
    | _ -> ZeroNatural

module NaturalInt = 
  let verify = function 
    | Natural i -> Some i
    | NonNatural _ -> None

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
  let assume = function
    | Natural i -> i
    | i -> invalidArg "i" (sprintf "Assertion failed trying to create a NaturalInt: %i >= 0" i)

module NegativeInt =  
  let verify = function
    | Negative i -> Some i
    | _ -> None  

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
  let assume = function 
    | Negative i -> i
    | i -> invalidArg "i" (sprintf "Assertion failed trying to create a NegativeInt: %i < 0" i)

module PositiveInt = 
  let private test i = i > 0

  let verify = function 
    | Positive i -> Some i
    | _ -> None

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
  let assume = function
    | Positive i -> i
    | i -> invalidArg "i" (sprintf "Assertion failed trying to create a PositiveInt: %i > 0" i)
    