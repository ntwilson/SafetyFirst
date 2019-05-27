module SafetyFirst.Specs.ConversionsSpec

open System
open NUnit.Framework
open Swensen.Unquote
open ResultDotNet

open SafetyFirst

[<Test>]
let ``converts int16 to byte by throwing overflowing or returning an option`` () = 
  test 
    <@ 
      Byte.tryOfInt16 12s = Some 12uy
      &&
      12s.ToByteSafe () = Ok 12uy
      &&
      Byte.unsafeOfInt16 12s = 12uy
      &&
      12s.ToByteUnsafe () = 12uy
      &&
      Byte.uncheckedOfInt16 12s = 12uy
      &&
      12s.ToByteUnchecked () = 12uy
      &&
      Byte.tryOfInt16 256s = None
      &&
      match 256s.ToByteSafe () with | Error _ -> true | _ -> false
      &&
      Byte.uncheckedOfInt16 256s = 0uy
      &&
      256s.ToByteUnchecked () = 0uy
    @>

  raises <@ Byte.unsafeOfInt16 256s @>  
  raises <@ 256s.ToByteUnsafe () @>

[<Test>]
let ``converts int32 to int16 by throwing overflowing or returning an option`` () = 
  test 
    <@ 
      Int16.tryOfInt32 12 = Some 12s
      &&
      12 .ToInt16Safe () = Ok 12s
      &&
      Int16.uncheckedOfInt32 12 = 12s
      &&
      12 .ToInt16Unchecked () = 12s
      &&
      Int16.unsafeOfInt32 12 = 12s
      &&
      12 .ToInt16Unsafe () = 12s
      &&
      Int16.tryOfInt32 32768 = None
      && 
      match 32768 .ToInt16Safe () with | Error _ -> true | _ -> false
      &&
      Int16.uncheckedOfInt32 32768 = -32768s
      &&
      32768 .ToInt16Unchecked () = -32768s 
    @>

  raises <@ Int16.unsafeOfInt32 32768 @>
  raises <@ Int16.unsafeOfInt32 32768 @>

[<Test>]
let ``converts byte to int16 with no coersion needed`` () = 
  test <@ Int16.ofByte 12uy = 12s @>

[<Test>]
let ``safely converts voption values to strings`` () = 
  test 
    <@
      str ValueNone = "ValueNone"
      &&
      str [ValueSome 5.0; ValueNone] = "[ValueSome 5.0; ValueNone]"
    @>

[<Test>]
let ``safely converts all other values to strings like normal`` () = 
  test 
    <@
      str None = string None
      &&
      str (Some [ Some {| X = "hi"; Y = None; Z = {1 .. 10} |}; None ]) = string (Some [ Some {| X = "hi"; Y = None; Z = {1 .. 10} |}; None ])
      &&
      str [| Some {| X = "hi"; Y = None; Z = {1 .. 10} |}; None |] = string [| Some {| X = "hi"; Y = None; Z = {1 .. 10} |}; None |]
    @>
