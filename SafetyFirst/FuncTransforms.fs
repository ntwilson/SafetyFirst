module internal ResultDotNet.FuncTransforms

open System

let toCSharpFunc f = Func<'a, 'b> f
let toFSharpFunc (f:Func<'a, 'b>) arg = f.Invoke arg
let toFSharpFunc2 (f:Func<'a, 'b, 'c>) arg1 arg2 = f.Invoke (arg1, arg2)
let toFSharpFunc3 (f:Func<'a, 'b, 'c, 'd>) arg1 arg2 arg3 = f.Invoke (arg1, arg2, arg3)
let toFSharpFunc4 (f:Func<'a, 'b, 'c, 'd, 'e>) arg1 arg2 arg3 arg4 = f.Invoke (arg1, arg2, arg3, arg4)
