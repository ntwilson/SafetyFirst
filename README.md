## SafetyFirst

This is an opinionated library for using monadic error handling from F# (or C#).  

### Overview

Monadic error handling is frankly contrary to the F# style guide, which says: 

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



### Collection Types

#### `FiniteSeq`

There are three main sequence types in F#: Array, List, and Seq.  Working with Lists makes any interop with C# intolerable.  Arrays are mutable, eager, and certain operations (like `tail` or `cons`) perform poorly.  Of course, we can choose whichever of the two is better suited for our use upon creation, and then use `Seq` everywhere else, but the `Seq` provides many challenges because of the fact that it _could_ be infinite.  The worst is that it has reference equality instead of structural equality (which means if you include it in any records or union types, those types lose their structural equality as well).  There are also several functions in the `Seq` module that should never be called for an infinite sequence, and you will only discover a violation of this at runtime (like `length`, `fold`, `rev`, etc.).  This library introduces the FiniteSeq type for a lazily evaluated sequence that is constrained to be finite.  It has structural equality, and can be constructed from any other sequence in _O(1)_ time, just like a `seq`.  The functions in the FiniteSeq module are safe for use with any FiniteSeq.  It's built on top of the `LazyList` type provided by the [FSharpx.Collections](http://fsprojects.github.io/FSharpx.Collections/) package.

The FiniteSeq type also uses the alias `fseq` and the FiniteSeq module also uses the alias `FSeq`

For example:

```f#
type TimeSeries = 
  { 
    Times : DateTime fseq
    Values : float fseq
  }

let rng = new Random ()
let values = 
  [0 .. 100] |> Seq.map (fun _ -> rng.NextDouble() * 100.0) 
  |> fseq |> FSeq.filter (fun i -> i > 50.0)
let times = 
  fseq { 0.0 .. (float (FSeq.length values)) } 
  |> FSeq.map (fun i -> startTime.AddHours i)
let ts = { Times = times; Values = values}
if (ts = someOtherTimeSeries) then ...
```



#### `InfiniteSeq`

To go along with FiniteSeq, it's helpful to have a type for representing a sequence that's known to be infinite.  Some functions are excluded from the InfiniteSeq module (like `fold`), while others are known to be safe for use (like `head`).  An InfiniteSeq can be created with the `InfiniteSeq.init` function (which behaves exactly like `Seq.initInfinite`), and the functions in the `InfiniteSeq` module are safe to use for infinite sequences (well, as safe as you can be.  You can certainly construct a sequence that will hang forever as soon as you try to do anything useful with it, e.g., the sequence: 

`InfiniteSeq.init (fun _ -> 0) |> InfiniteSeq.filter ((<>) 0)` will hang if you were to use any eager calculations with it, like `take` or `head` or `find`).



#### `NonEmptySeq`

It's helpful to have a type representing a sequence that's known to be non-empty.  This type is built on top of FiniteSeq (if your sequence is infinite, you should use the InfiniteSeq type instead, which is automatically assumed to be non-empty).  There are two ways to get a NonEmptySeq.  One is a pattern matcher with any seq (though if the seq is infinite, be aware that the returned value is built on FiniteSeq and in danger of hanging infinitely for certain operations):

```f#
match xs with // `xs` is any seq, but should be finite
| Empty -> ...
| NotEmpty nes -> ... // `nes` is a NonEmptySeq
```

The other is to construct it with a head and tail

```f#
let nes = NonEmptySeq.create 0 (fseq {1 .. 10})
```

NonEmptySeq contains several functions that aren't included in the FiniteSeq module because they could throw, such as `head`, `tail`, `uncons`, and `reduce`.