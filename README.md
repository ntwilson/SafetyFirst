## SafetyFirst

This is an opinionated library for using monadic error handling from F# (or C#).  This is frankly contrary to the F# style guide, which says: 

> ### Do not use monadic error handling to replace exceptions

 \- https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/conventions#do-not-use-monadic-error-handling-to-replace-exceptions

Monadic error handling is when code that would normally return an `'a` or throw an exception instead returns a `Result<'a, 'errorType>` and never throws.  (The Result type is a monad and can be composed as such, which is where the name comes from).  It essentially makes a form of checked exception, because the caller has to deal with the fact that the return type includes the possibility of an error instead of the plain value.

This library takes the opinion that monadic error handling can actually be a better default approach to errors than throwing _as long as there's an easy way to get back to exceptions_.  Monadic error handling, same as checked exceptions, can get a bad name because it's so inconvenient when an error can't be handled (except to crash, log the problem, and inform the user).  If checked exceptions or the Result type were made more convenient, it becomes worthwhile to make the return type of a function reflect the possibility of error.

This library pulls from [ResultDotNet](https://www.nuget.org/packages/ResultDotNet) and [OptionExt](https://www.nuget.org/packages/OptionExt) to add a few helper functions to make it easy to work with the Result type and the Option type.  For both types, the `unless` function let's you "extract" the value assuming everything worked, provided a message that explains the problem.  For example, instead of 

```F#
let vip = customers |> Seq.maxBy (fun cust -> cust.TotalSales)
```

which could throw the quite unhelpful:

```f#
System.ArgumentException: The input sequence was empty.
Parameter name: source
```

you would use:

```F#
let vip = 
  customers |> Seq.maxBySafe (fun cust -> cust.TotalSales)
  |> Result.unless "No customers have been loaded into the system"
```

This has a higher development cost, but also provides a lot more information to the reader as well as the potential debugger when a problem arises (that string gets included in the exception if one is thrown).

Since the Result type already contains error information in it, the Result type also has an `expect` function that functions just like `unless` but doesn't require an additional message.  So you could do:

```F#
let vip = customers |> Seq.maxBySafe (fun cust -> cust.TotalSales) |> Result.expect
```

which has almost no cost, but informs the programmer (and the reader) that `maxBy` could error, and that we're just assuming that it will work.  This will end up throwing an equally unhelpful exception as `maxBy` though.

Using `unless` is recommended over using `expect`, since the further up the call stack you are, the more helpful an error message can be assembled, but `expect` still gives you some of the benefit of making possible errors more visible without much cost.

This library will add `<function>Safe` functions to many of the modules in the FSharp.Core and Linq libraries:

> TODO: fill in functions here

From F#, there is also an alias using the postfix apostrophe (`<function>'`) for functions that return Results instead of throwing.  So you can use `Seq.head'` as a shorthand for `Seq.headSafe`.   

> NOTE:
>
> This library doesn't bother with monadic error handling for any IO (working with files, database, network, etc.) for a few reasons.
>
> 1) For IO it is always obvious that an error is possible, and making that clear to the programmer/reader/debugger isn't helpful.  Program logic that throws is often much less obvious.
>
> 2) The scope of all available IO functions that could throw is beyond what this library is intending to accomplish.
>
> 3) Composing Results with different possible errors is still cumbersome.  IO code will typically involve many calls with many different possible errors.
>
> 4) IO code is necessarily imperative rather than functional.  Exceptions have proven themselves as the optimal solution for imperative code.

