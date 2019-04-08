[![Build Status](https://travis-ci.com/ntwilson/SafetyFirst.svg?branch=master)](https://travis-ci.com/ntwilson/SafetyFirst)

## SafetyFirst

This is an opinionated library for avoiding partial functions primarily for F# (also for C#).  

### Overview

Partial functions are functions that are undefined for only _some_ of their inputs, throwing
an exception for invalid inputs.  Making a partial function total involves one of 
two strategies.  You can either adjust the output to account for an error condition (by returning an `Option<_>` or `Result<_,_>`), or you can restrict the inputs to only the valid ones.  This library aims to aid with both strategies.

The main advantage of using total functions is that the compiler can guarantee the absense of runtime errors for your program _logic_.  (This only applies here to logic as pure functions.  It doesn't apply to IO or other impure functions. This library holds the same opinion as the [F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/conventions#do-not-use-monadic-error-handling-to-replace-exceptions) that Exceptions are the best mechanism for handling errors when interacting with the outside world).  Languages such as [Elm](https://elm-lang.org/) boast the ability to create programs that never suffer a runtime exception using this technique.  This is because the compiler lets the programmer know about every possible error so that they can be dealt with properly with no surprises.

F# is a pragmatic language, however, and this library holds the opinion that to be effective, back doors must be available, so we prioritize making it simple to get back to an Exception.  Total functions (same as checked exceptions) can get a bad name because they can be so inconvenient when a bad input simply can't be handled (except to crash, log the problem, and inform the developer or user).

Let's look at an example.  `Seq.maxBy` is a partial function because it's only defined for sequences that aren't empty.  It will throw for an empty list.  So let's consider the following unsafe code:

```F#
let vip (salesReps:SalesRep seq) : SalesRep = 
  salesReps |> Seq.maxBy (fun rep -> rep.TotalSales)
```

If an empty seq is passed in, it would throw the quite unhelpful:

```f#
System.ArgumentException: The input sequence was empty.
Parameter name: source
```

Not to mention that the developer writing (or reading) this function may not even be aware that there's a problem that would need to be caught.

One way to make this total would be to adjust the output to reflect the possibility of an error:

```F#
let vip (salesReps:SalesRep seq) : Result<SalesRep, SeqIsEmpty> = 
  salesReps |> Seq.maxBy' (fun rep -> rep.TotalSales)
```

By convention, this library uses an apostrophe after a function name for a function that returns a `Result<_,_>`.  We try to make the name match any existing functions that would otherwise throw.  (We also provide the equivalent functions with the `Safe` suffix instead of an apostrophe, e.g., `Seq.maxBySafe`.  This is the only function available for use from C#, where apostrophes are not allowed in function names).

Of course the normal approach when calling the `vip` function would be to pattern match on the output and provide an execution path for what to do with an `Error` (in this case from an empty seq).  But it may be that your application just doesn't allow for an empty seq of sales reps at all.  This is a case where getting back to Exceptions is helpful, since we don't want to handle the possibility of an empty seq, we want to just crash and inform the user or developer that you need to provide at least one sales rep.  In fact, if we can't handle the problem, and the only thing to do is crash the application or subsystem, the _preferred_ approach is to switch to an Exception instead of propagating `Result<_,_>`s all the way up.

This library pulls from [ResultDotNet](https://www.nuget.org/packages/ResultDotNet) and [OptionExt](https://www.nuget.org/packages/OptionExt) to add a few helper functions to make it easy to work with the Result type and the Option type.  For both types, the `unless` function let's you "extract" the value assuming everything worked, provided a message that explains the problem.  Here we could do 

```F#
let vip (salesReps:SalesRep seq) : SalesRep = 
  salesReps 
  |> Seq.maxBy' (fun cust -> cust.TotalSales)
  |> Result.unless "No sales reps have been loaded into the system.  You must provide at least one sales rep"
```

This has a higher development cost than using the partial `maxBy` function, but if the exception is thrown, it provides a better error message because we now know a lot more about the context of the problem than we would from inside the maxBy function.  We know that specifically our seq of SalesReps is empty, and can reflect that in the error message.  Also this provides a lot more information to the reader of the `vip` function because it's obvious what the assumptions this function makes are (in this case, that the input includes at least one sales rep).

Since the Result type already contains error information in it, the Result type also has an `expect` function that functions just like `unless` but doesn't require an additional message.  So you could do:

```F#
let vip (salesReps:SalesRep seq) : SalesRep = 
  salesReps |> Seq.maxBySafe (fun cust -> cust.TotalSales) |> Result.expect
```

which has almost no cost compared to the partial `maxBy` function, but informs the programmer (and the reader) that `maxBy` could error, and that we're just assuming that it will work.  If an Exception does occur though, this will end up throwing an equally unhelpful exception as `maxBy`.

But if we really don't allow for an empty set of sales reps, then maybe we want to explore the _other_ option for making this function total: restricting the inputs to only valid inputs.  Here, we could use the `NonEmpty` type to restrict the input to only allow a non-empty set of sales reps:

```F#
let vip (salesReps:NonEmptySeq<SalesRep>) : SalesRep = 
  salesReps |> Seq.NonEmpty.maxBy (fun rep -> rep.TotalSales)
```

`Seq.NonEmpty.maxBy` doesn't ever throw an exception, because you can't create a `NonEmptySeq` unless you have at least one element.  `NonEmpty` has a pattern matcher to create types like `NonEmptySeq`:

```F#
let salesRepsFromDB = DBContext.SalesReps |> Seq.map SalesRep.fromEF |> Seq.toArray
let salesReps : NonEmptyArray<SalesRep> = 
  match salesRepsFromDB with
  | NonEmpty x -> x
  | Empty -> failwith "The database contains no sales reps.  At least one sales rep is needed."
```
(or even) 
```F#
let salesReps : NonEmptyArray<SalesRep> = 
  DBContext.SalesReps |> Seq.map SalesRep.fromEF |> Seq.toArray
  |> NonEmpty.verify //returns an Option<NonEmpty<_>>
  |> Option.unless "The database contains no sales reps.  At least one sales rep is needed."
```

Now we have one failure spot for an empty set of sales reps that occurs just where it's read in from the database, and from then on, the compiler keeps track of the fact that it isn't empty.  This is analogous to checking for null when first loading the data, instead of including null checks in every function (now possible in C# too with the new nullable reference types).

Below is the set of partial functions in FSharp.Core that this library provides total versions of by returning a `Result<_,_>` instead of throwing. 

#### F# Functions

- `Seq/List/Array.average`
- `Seq/List/Array.averageBy`
- `Seq/List/Array.chunkBySize`
- `Seq/List/Array.exactlyOne`
- `Seq/List/Array.find`
- `Seq/List/Array.findBack`
- `Seq/List/Array.findIndex`
- `Seq/List/Array.findIndexBack`
- `Seq/List/Array.head`
- `Seq/List/Array.item`
- `Seq/List/Array.last`
- `Seq/List/Array.max`
- `Seq/List/Array.maxBy`
- `Seq/List/Array.min`
- `Seq/List/Array.minBy`
- `Seq/List/Array.pick`
- `Seq/List/Array.reduce`
- `Seq/List/Array.reduceBack`
- `Seq/List/Array.skip`
- `Seq/List/Array.splitInto`
- `Seq/List/Array.tail`
- `Seq/List/Array.take`
- `Seq/List/Array.windowed`
- `List/Array.fold2`
- `List/Array.foldBack2`
- `List/Array.forall2`
- `List/Array.iter2`
- `List/Array.iteri2`
- `List/Array.map2`
- `List/Array.mapi2`
- `List/Array.map3`
- `List/Array.splitAt`
- `List/Array.zip`
- `List/Array.zip3`
- `Array.sub`
- `Map.ofSeq`
- `Map.ofList`
- `Map.ofArray`

Below is the set of C# LINQ partial functions that this library provides total versions of by returning a `Result<_,_>` instead of throwing.

#### LINQ Functions

- `Aggregate`
- `Average`
- `ElementAt`
- `First`
- `Last`
- `Max`
- `Min`
- `Single`
- `Zip`

> NOTE:
>
> This library doesn't bother with altering error handling for any IO (working with files, database, network, etc.) for a few reasons.
>
> 1) For IO it is always obvious that an error is possible, and making that clear to the programmer/reader/debugger isn't helpful.  Program _logic_ that throws is often much less obvious.
>
> 2) The scope of all available IO functions that could throw is beyond what this library is intending to accomplish.
>
> 3) Composing Results with different possible errors is still very cumbersome compared to Exceptions.  IO code will typically involve many calls with many different possible errors.
>
> 4) IO code is necessarily imperative rather than functional.  Exceptions have proven themselves as the optimal solution for imperative code.


### Restricted Types

These are types that this library creates to have a restricted set of possible values than it's parent type. We've already explored the NonEmpty set of collections briefly, which restricts a collection type to only allow values that contain at least one value.  This library provides several, but the technique is fairly straightforward, so you are also encouraged to create your own when necessary.  The [Numbers module](https://github.com/ntwilson/SafetyFirst/blob/master/SafetyFirst/Numbers.fs) (described below) is a good example if you want to create similar types in your own codebase.

#### Collections

##### `FiniteSeq`

This one is unique in that there is no runtime check we can do to verify that a sequence is finite, so it's entirely based on developer assertions that the seq is finite.  Marking a sequence finite is an important distinction since some functions can be safely called on any infinite list, but would be partial for finite lists (like `take` or `skip` or indexing).  Conversely there are some functions that can be safely called on any finite list, but would diverge for infinite lists (like `fold` and everything that derives from `fold` like `max`/`min`, `last`, `reduce`, `sum`, `length`, `exists`, `forall`, etc.) (interestingly, `forall` and `exists` can return in some circumstances from an infinite list, but `forall` can never produce the value `true`, only `false`, and `exists` can never produce the value `false`, only `true`, so applying them to an infinite list is senseless).

Finite sequences are particularly important, since they can have _value equality_ (two finite sequences that contain the same values are equal) instead of _referential equality_ like `seq` (two sequences are only equal if they point to the same memory address).  (`seq` cannot have value equality because comparing equality of infinite seqs could diverge).

There are three main sequence types in F#: Array, List, and seq.  There are similarly three main sequence types in C#: Array, List, and IEnumerable (where, somewhat confusingly, when I say "List" from F# and C#, I'm talking about _different_ types, but when I say "seq" and "IEnumerable", I'm talking about the same type). Working with F# Lists makes any interop with C# intolerable.  Arrays are mutable, eager, and certain operations (like `tail` or `cons`) perform poorly.  C# lists are similarly mutable and eager, and make F# interop difficult.  Of course, we can choose whichever is best suited for our use upon creation, and then use `seq`/`IEnumerable` everywhere else, but this provides many challenges because of the fact that our `seq` _could_ be infinite.  The worst is the reference equality instead of structural equality mentioned above (this also means if you include a seq in any records or union types, those types lose their structural equality as well).  This library introduces the `FiniteSeq` type for a lazily evaluated sequence that is constrained to be finite.  It has structural equality, and can be constructed from any other sequence in _O(1)_ time, just like a `seq`/`IEnumerable`.  Even from C#, two `FiniteSeq` instances will be equal if they contain the same elements.  The functions in the `FiniteSeq` module are safe for use with any `FiniteSeq`.  It's built on top of the `LazyList` type provided by the [FSharpx.Collections](http://fsprojects.github.io/FSharpx.Collections/) package.  Note that from C#, this is one of the more convenient ways of caching an `IEnumerable` but keeping it lazy.  You can use the `.Finite()` extension method to make any `IEnumerable` into a `FiniteSeq`.

The `FiniteSeq` type also uses the alias `fseq` and the `FiniteSeq` module also uses the alias `FSeq`

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


##### `InfiniteSeq`

To go along with FiniteSeq, it's helpful to have a type for representing a sequence that's known to be infinite.  Some functions are excluded from the InfiniteSeq module (like `fold`), while others are known to be safe for use (like `head`).  An InfiniteSeq can be created with the `InfiniteSeq.init` function (which behaves exactly like `Seq.initInfinite`), and the functions in the `InfiniteSeq` module are safe to use for infinite sequences (well, as safe as you can be.  You can certainly construct a sequence that will hang forever as soon as you try to do anything useful with it, e.g., the sequence: 

`InfiniteSeq.init (fun _ -> 0) |> InfiniteSeq.filter ((<>) 0)` will hang if you were to use any eager calculations with it, like `take` or `head` or `find`).

Note that from C#, you can use the InfiniteSeq module directly, but InfiniteSeq doesn't add much benefit if you're using LINQ style syntax.  Since InfiniteSeq is an `IEnumerable`, you'll still see all of the regular LINQ extension methods, which will include all of the methods that should never be called on an infinite sequence.


##### `NonEmpty`

We've already been introduced to the `NonEmpty` type.  It's actually a wrapper for _any_ type of sequence, but the necessary type signature is a bit ugly (e.g., `NonEmpty<seq<int>, int>`) which is needed to satisfy .NET's type system, so there are aliases for many common types: `NonEmptySeq<'a>`, `NonEmptyArray<'a>`, `NonEmptyFSeq<'a>`, and `NonEmptyList<'a>` (that's an F# list).  Feel free to use it with any other type of collection however.  We also add modules `Seq.NonEmpty`, `Array.NonEmpty`, `FSeq.NonEmpty`, and `List.NonEmpty` for functions that take in a `NonEmpty` and have their signatures modified to reflect that.  (There's also `InfiniteSeq.asNonEmpty` which returns a `NonEmpty<InfiniteSeq<'a>, 'a>` in case you need to pass an infinite seq to a function taking a `NonEmpty`). There are two ways to get a `NonEmpty`.  One is a pattern matcher with any seq:

```f#
match xs with // `xs` is any seq
| Empty -> ...
| NotEmpty nes -> ... // `nes` is a NonEmpty<_>
```

The other is to construct it with a head and tail

```f#
let nes = List.NonEmpty.create 0 [1 .. 10]
```

#### Number Types

Several functions rely on an integer being >= 0 or > 0.  This library defines types for these numbers.  A `NaturalInt` is an int constrained to be >= 0.  A `PositiveInt` is constrained to be > 0.  A `NegativeInt` is constrained to be < 0.  Several functions will offer one version that will return a `Result<_,_>` if given a negative input, and another version that only accepts either a `PositiveInt` or `NaturalInt` (depending on the function's needs).  You can create these types in a variety of ways.
1) pattern matching:
```F#
let x = 
  match i with
  | Positive p -> //p is a PositiveInt
  | Negative n -> //n is a NegativeInt
  | Zero -> ...

let y =
  match i with
  | Natural n -> //n is a NaturalInt
  | NonNatural nn -> //nn is a NegativeInt
```
2) the `verify` functions: 
```F#
let x : PositiveInt option = PositiveInt.verify x
```
(keep in mind that can be used with the `Option.unless` function):
```F#
let x : PositiveInt = PositiveInt.verify x |> Option.unless (sprintf "'x' (%i) isn't allowed to be < 1" x)
```
3) the `assume` functions:
```F#
let x : NaturalInt = NaturalInt.assume 5
```
The `assume` functions are partial, but are helpful for cases where the int so obviously satisfies the constraint that using pattern matching or an `unless` function would be laughable:
```F#
let x : PositiveInt = PositiveInt.verify 5 |> Option.unless "5 is negative?"
let y : NaturalInt = xs |> List.length |> NaturalInt.verify |> Option.unless "list has a negative length?"
```

Functions that utilize `PositiveInt` include `chunksOf` (a safe version of `chunkBySize`), `splitIntoN` (a safe version of `splitInto`), and `window` (a safe version of `windowed`).  Functions that utilize `NaturalInt` include `drop` and `skip`.