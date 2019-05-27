namespace SafetyFirst

open System

module StringConversionHelpers = 
  let rec isVOptionOrListContainingVOptions (t:System.Type) = 
    if t.IsGenericType then
      let td = t.GetGenericTypeDefinition ()
      if td = typedefof<int voption> then true
      elif td = typedefof<int list> then
        let elementType = Array.head (t.GetGenericArguments ())
        isVOptionOrListContainingVOptions elementType 
      else false
    else false


[<AutoOpen>]
module Conversions =

  /// <summary>
  /// Functions very similarly to the regular <c>string</c> function, but safely handles ValueNone,
  /// or lists containing ValueNone.  Since the <c>string</c> function can 
  /// raise an exception when given a ValueOption or a list containing a ValueOption.
  /// </summary>
  let inline str x = 
    if isNull (box x) then string x
    else
      let t = x.GetType ()
      if StringConversionHelpers.isVOptionOrListContainingVOptions t then sprintf "%A" x
      else string x

  /// <summary>
  /// An alias for the regular <c>string</c> function.  Since the <c>string</c> function can 
  /// raise an exception when given a ValueOption or a list containing a ValueOption, it is 
  /// marked Unsafe.  Use the safe <c>str</c> function to safely convert any value to a string.
  /// </summary>
  let inline stringUnsafe x = string x


  type Byte with
    /// <summary>
    /// Converts an Int16 to a byte.  Will overflow if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline uncheckedOfInt16 (s:int16) = byte s

    /// <summary>
    /// Converts an Int16 to a byte.  Will throw if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline unsafeOfInt16 (s:int16) = Checked.byte s

    /// <summary>
    /// Converts an Int16 to a byte.  Returns None if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline tryOfInt16 (s:int16) =
      if s <= int16 (Byte.MaxValue) && s >= int16 (Byte.MinValue)
      then Some <| byte s
      else None

    /// <summary>
    /// Converts an Int32 to a byte.  Will overflow if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline uncheckedOfInt32 (i:int) = byte i

    /// <summary>
    /// Converts an Int32 to a byte.  Will throw if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline unsafeOfInt32 (i:int) = Checked.byte i

    /// <summary>
    /// Converts an Int32 to a byte.  Returns None if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline tryOfInt32 (i:int) =
      if i <= int (Byte.MaxValue) && i >= int (Byte.MinValue)
      then Some <| byte i
      else None

    /// <summary>
    /// Converts an Int64 to a byte.  Will overflow if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline uncheckedOfInt64 (i:int64) = byte i

    /// <summary>
    /// Converts an Int64 to a byte.  Will throw if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline unsafeOfInt64 (i:int64) = Checked.byte i

    /// <summary>
    /// Converts an Int64 to a byte.  Returns None if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline tryOfInt64 (i:int64) =
      if i <= int64 (Byte.MaxValue) && i >= int64 (Byte.MaxValue)
      then Some <| byte i
      else None

    /// <summary>
    /// Converts a float to a byte.  Will overflow if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline uncheckedOfFloat (f:float) = byte f

    /// <summary>
    /// Converts a float to a byte.  Will throw if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline unsafeOfFloat (f:float) = Checked.byte f

    /// <summary>
    /// Converts a float to a byte.  Returns None if given a value outside of the range
    /// of possible values that a byte can hold.
    /// </summary>
    static member inline tryOfFloat (f:float) =
      if f <= float Byte.MaxValue && f >= float Byte.MinValue
      then Some <| byte f
      else None

  type Int16 with
    static member inline ofByte (b:byte) = int16 b

    /// <summary>
    /// Converts an Int32 to an Int16.  Will overflow if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline uncheckedOfInt32 (i:int) = int16 i

    /// <summary>
    /// Converts an Int32 to an Int16.  Will throw if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline unsafeOfInt32 (i:int) = Checked.int16 i

    /// <summary>
    /// Converts an Int32 to an Int16.  Returns None if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline tryOfInt32 (i:int) =
      if i <= int (Int16.MaxValue) && i >= int (Int16.MinValue)
      then Some <| int16 i
      else None

    /// <summary>
    /// Converts an Int64 to an Int16.  Will overflow if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline uncheckedOfInt64 (i:int64) = int16 i

    /// <summary>
    /// Converts an Int64 to an Int16.  Will throw if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline unsafeOfInt64 (i:int64) = Checked.int16 i

    /// <summary>
    /// Converts an Int64 to an Int16.  Returns None if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline tryOfInt64 (i:int64) =
      if i <= int64 (Int16.MaxValue) && i >= int64 (Int16.MaxValue)
      then Some <| int16 i
      else None

    /// <summary>
    /// Converts a float to an Int16.  Will overflow if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline uncheckedOfFloat (f:float) = int16 f

    /// <summary>
    /// Converts a float to an Int16.  Will throw if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline unsafeOfFloat (f:float) = Checked.int16 f

    /// <summary>
    /// Converts a float to an Int16.  Returns None if given a value outside of the range
    /// of possible values that an Int16 can hold.
    /// </summary>
    static member inline tryOfFloat (f:float) =
      if f <= float Int16.MaxValue && f >= float Int16.MinValue
      then Some <| int16 f
      else None

  type Int32 with
    static member inline ofByte (b:byte) = int b
    static member inline ofInt16 (s:int16) = int s

    /// <summary>
    /// Converts a float to an Int32.  Will overflow if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline uncheckedOfFloat (f:float) = int f

    /// <summary>
    /// Converts a float to an Int32.  Will throw if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline unsafeOfFloat (f:float) = Checked.int f

    /// <summary>
    /// Converts a float to an Int32.  Returns None if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline tryOfFloat (f:float) =
      if f <= float Int32.MaxValue && f >= float Int32.MinValue
      then Some <| int f
      else None

    /// <summary>
    /// Converts an Int64 to an Int32.  Will overflow if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline uncheckedOfInt64 (i:int64) = int i

    /// <summary>
    /// Converts an Int64 to an Int32.  Will throw if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline unsafeOfInt64 (i:int64) = Checked.int i

    /// <summary>
    /// Converts an Int64 to an Int32.  Returns None if given a value outside of the range
    /// of possible values that an Int32 can hold.
    /// </summary>
    static member inline tryOfInt64 (i:int64) =
      if i <= int64 Int32.MaxValue && i >= int64 Int32.MinValue
      then Some <| int i
      else None

  type Int64 with
    static member inline ofByte (b:byte) = int b
    static member inline ofInt16 (s:int16) = int s
    static member inline ofInt32 (i:int32) = int64 i

    /// <summary>
    /// Converts a float to an Int64.  Will overflow if given a value outside of the range
    /// of possible values that an Int64 can hold.
    /// </summary>
    static member inline uncheckedOfFloat (f:float) = int64 f

    /// <summary>
    /// Converts a float to an Int64.  Will throw if given a value outside of the range
    /// of possible values that an Int64 can hold.
    /// </summary>
    static member inline unsafeOfFloat (f:float) = Checked.int64 f

    /// <summary>
    /// Converts a float to an Int64.  Returns None if given a value outside of the range
    /// of possible values that an Int64 can hold.
    /// </summary>
    static member inline tryOfFloat (f:float) =
      if f <= float Int64.MaxValue && f >= float Int64.MinValue
      then Some <| int64 f
      else None

  module Float32 =
    let inline ofByte  (b:byte)  = float b
    let inline ofInt16 (s:int16) = float s
    let inline ofInt32 (i:int32) = float i
    let inline ofInt64 (i:int64) = float i

    /// <summary>
    /// Converts a float to an float32.  Will overflow if given a value outside of the range
    /// of possible values that an float32 can hold.
    /// </summary>
    let inline uncheckedOfFloat (f:float) = float32 f

    /// <summary>
    /// Converts a float to an float32.  Returns None if given a value outside of the range
    /// of possible values that an float32 can hold.
    /// </summary>
    let inline tryOfFloat (f:float) =
      if f <= float Single.MaxValue && f >= float Single.MinValue
      then Some <| float32 f
      else None

    /// <summary>
    /// Converts a float to an float32.  Will throw if given a value outside of the range
    /// of possible values that an float32 can hold.
    /// </summary>
    let inline unsafeOfFloat (f:float) = 
      match tryOfFloat f with
      | None -> raise <| OverflowException ()
      | Some x -> x

  module Float =
    let inline ofByte  (b:byte)  = float b
    let inline ofInt16 (s:int16) = float s
    let inline ofInt32 (i:int32) = float i
    let inline ofInt64 (i:int64) = float i
    let inline ofFloat32 (f:float32) = float f


// The rest of this file is for C# conversions
// This doesn't create any functions for safe conversions,
// since C# has all safe conversions as implicit.  For example,
// you can pass an int into a function that takes a double,
// but you can't pass a double into a function that takes an int
// without some kind of explicit conversion.  This file only has
// explicit conversions. 

open System.Runtime.CompilerServices
open ResultDotNet

[<AutoOpen>]
[<Extension>]
type Int16Extensions private () =
  /// <summary>
  /// Converts to a byte.  Will overflow if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnchecked (s:int16) = byte s

  /// <summary>
  /// Converts to a byte.  Will throw if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnsafe (s:int16) = Checked.byte s

  /// <summary>
  /// Converts to a byte.  Will overflow if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteSafe (s:int16) =
    if s <= int16 (Byte.MaxValue) && s >= int16 (Byte.MinValue)
    then Ok <| byte s
    else Error <| OverflowException ()

[<AutoOpen>]
[<Extension>]
type Int32Extensions private () =
  /// <summary>
  /// Converts to a byte.  Will overflow if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnchecked (i:int) = byte i

  /// <summary>
  /// Converts to a byte.  Will throw if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnsafe (i:int) = Checked.byte i

  /// <summary>
  /// Converts to a byte.  Returns None if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteSafe (i:int) =
    if i <= int (Byte.MaxValue) && i >= int (Byte.MinValue)
    then Ok <| byte i
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int16.  Will overflow if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unchecked (i:int) = int16 i

  /// <summary>
  /// Converts to an Int16.  Will throw if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unsafe (i:int) = Checked.int16 i

  /// <summary>
  /// Converts to an Int16.  Returns None if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Safe (i:int) =
    if i <= int (Int16.MaxValue) && i >= int (Int16.MinValue)
    then Ok <| int16 i
    else Error <| OverflowException ()

[<AutoOpen>]
[<Extension>]
type Int64Extensions private () =
  /// <summary>
  /// Converts to a byte.  Will overflow if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnchecked (i:int64) = byte i

  /// <summary>
  /// Converts to a byte.  Will throw if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnsafe (i:int64) = Checked.byte i

  /// <summary>
  /// Converts to a byte.  Returns None if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteSafe (i:int64) =
    if i <= int64 (Byte.MaxValue) && i >= int64 (Byte.MaxValue)
    then Ok <| byte i
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int16.  Will overflow if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unchecked (i:int64) = int16 i

  /// <summary>
  /// Converts to an Int16.  Will throw if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unsafe (i:int64) = Checked.int16 i

  /// <summary>
  /// Converts to an Int16.  Returns None if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Safe (i:int64) =
    if i <= int64 (Int16.MaxValue) && i >= int64 (Int16.MaxValue)
    then Ok <| int16 i
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int32.  Will overflow if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Unchecked (i:int64) = int i

  /// <summary>
  /// Converts to an Int32.  Will throw if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Unsafe (i:int64) = Checked.int i

  /// <summary>
  /// Converts to an Int32.  Returns None if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Safe (i:int64) =
    if i <= int64 Int32.MaxValue && i >= int64 Int32.MinValue
    then Ok <| int i
    else Error <| OverflowException ()

[<AutoOpen>]
[<Extension>]
type DoubleExtensions private () =
  /// <summary>
  /// Converts to a byte.  Will overflow if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnchecked (f:float) = byte f

  /// <summary>
  /// Converts to a byte.  Will throw if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteUnsafe (f:float) = Checked.byte f

  /// <summary>
  /// Converts to a byte.  Returns None if given a value outside of the range
  /// of possible values that a byte can hold.
  /// </summary>
  [<Extension>]
  static member ToByteSafe (f:float) =
    if f <= float Byte.MaxValue && f >= float Byte.MinValue
    then Ok <| byte f
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int16.  Will overflow if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unchecked (f:float) = int16 f

  /// <summary>
  /// Converts to an Int16.  Will throw if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Unsafe (f:float) = Checked.int16 f

  /// <summary>
  /// Converts to an Int16.  Returns None if given a value outside of the range
  /// of possible values that an Int16 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt16Safe (f:float) =
    if f <= float Int16.MaxValue && f >= float Int16.MinValue
    then Ok <| int16 f
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int32.  Will overflow if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Unchecked (f:float) = int f

  /// <summary>
  /// Converts to an Int32.  Will throw if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Unsafe (f:float) = Checked.int f

  /// <summary>
  /// Converts to an Int32.  Returns None if given a value outside of the range
  /// of possible values that an Int32 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt32Safe (f:float) =
    if f <= float Int32.MaxValue && f >= float Int32.MinValue
    then Ok <| int f
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to an Int64.  Will overflow if given a value outside of the range
  /// of possible values that an Int64 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt64Unchecked (f:float) = int64 f

  /// <summary>
  /// Converts to an Int64.  Will throw if given a value outside of the range
  /// of possible values that an Int64 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt64Unsafe (f:float) = Checked.int64 f

  /// <summary>
  /// Converts to an Int64.  Returns None if given a value outside of the range
  /// of possible values that an Int64 can hold.
  /// </summary>
  [<Extension>]
  static member ToInt64Safe (f:float) =
    if f <= float Int64.MaxValue && f >= float Int64.MinValue
    then Ok <| int64 f
    else Error <| OverflowException ()

  /// <summary>
  /// Converts to a Single.  Will overflow if given a value outside of the range
  /// of possible values that a Single can hold.
  /// </summary>
  [<Extension>]
  static member ToSingleUnchecked (f:float) = float32 f

  /// <summary>
  /// Converts to a Single.  Will throw if given a value outside of the range
  /// of possible values that a Single can hold.
  /// </summary>
  [<Extension>]
  static member ToSingleUnsafe (f:float) = Float32.unsafeOfFloat f

  /// <summary>
  /// Converts to a Single.  Returns None if given a value outside of the range
  /// of possible values that a Single can hold.
  /// </summary>
  [<Extension>]
  static member ToSingleSafe (f:float) =
    if f <= float Single.MaxValue && f >= float Single.MinValue
    then Ok <| float32 f
    else Error <| OverflowException ()



