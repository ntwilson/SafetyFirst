namespace SafetyFirst 

open System

open FSharpx

type SeqIsEmpty = SeqIsEmpty of string
type NotEnoughElements = NotEnoughElements of string
type NoMatchingElement = NoMatchingElement of string
type DifferingLengths = DifferingLengths of string
type NegativeInput = NegativeInput of string
type IndexOutOfRange = IndexOutOfRange of string

type WrongNumberOfElements = 
  | TooManyElements of string
  | NotEnoughElements of string

  member this.Message =
    match this with
    | TooManyElements s -> s
    | NotEnoughElements s -> s

  member this.Match (tooManyElements, notEnoughElements) = 
    match this with
    | TooManyElements s -> FSharpFunc.FromFunc<string, 'a> tooManyElements s
    | NotEnoughElements s -> FSharpFunc.FromFunc<string, 'a> notEnoughElements s

  member this.Match (tooManyElements, notEnoughElements) =
    match this with
    | TooManyElements s -> FSharpFunc.FromAction<string> tooManyElements s
    | NotEnoughElements s -> FSharpFunc.FromAction<string> notEnoughElements s


module internal ErrorTypes = 
  let private differingLengthsErr fName length1 length2 = 
    DifferingLengths <| sprintf "Cannot call %s with two sequences that have different lengths (%i and %i)" fName length1 length2

  let private differingLengthsErr3 fName length1 length2 length3 = 
    DifferingLengths <| sprintf "Cannot call %s with three sequences that have different lengths (%i, %i, and %i)" fName length1 length2 length3

  let inline avgErr () = SeqIsEmpty "Cannot get the average value of an empty sequence"
  let chunkErr = NegativeInput "Cannot chunkBySize with a negative size"
  let moreThanOneErr = TooManyElements "Sequence was expected to have a single element, but has more than one"
  let lessThanOneErr = NotEnoughElements "Sequence was expected to have a single element, but was empty"
  let findErr = NoMatchingElement "Cannot find an element in the input collection matching the input predicate"
  let fold2Err length1 length2 = differingLengthsErr "fold2" length1 length2
  let forall2Err length1 length2 = differingLengthsErr "forall2" length1 length2
  let headErr = SeqIsEmpty "Cannot get the first element (head) of an empty sequence"
  let iter2Err length1 length2 = differingLengthsErr "iter2" length1 length2
  let iteri2Err length1 length2 = differingLengthsErr "iteri2" length1 length2
  let lazyIndexTooLargeErr index = IndexOutOfRange <| sprintf "Cannot access the element at index %i in the input sequence because the sequence doesn't have enough elements" index
  let indexTooLargeErr index length = IndexOutOfRange <| sprintf "Cannot access the element at index %i in the input sequence because the sequence only contains %i elements" index length
  let indexNegativeErr index = IndexOutOfRange <| sprintf "Cannot access the element at a negative index value (%i used)" index
  let namedIndexNegativeErr indexName indexValue = IndexOutOfRange <| sprintf "Cannot access elements at a negative index value (%i was used for %s)" indexValue indexName
  let lastErr = SeqIsEmpty "Cannot retrieve the last element of an empty sequence"
  let map2Err length1 length2 = differingLengthsErr "map2" length1 length2
  let mapi2Err length1 length2 = differingLengthsErr "mapi2" length1 length2
  let map3Err length1 length2 length3 = differingLengthsErr3 "map3" length1 length2 length3
  let maxErr = SeqIsEmpty "Cannot produce a maximum element from an empty sequence"
  let minErr = SeqIsEmpty "Cannot produce a minimum element from an empty sequence"
  let pickErr = NoMatchingElement "Cannot find an element in the input collection matching the input predicate"
  let reduceErr = SeqIsEmpty "Cannot reduce an empty sequence"
  let lazySkipErr n = NotEnoughElements <| sprintf "Cannot skip %i elements of the input collection, since it does not contain enough elements" n
  let skipErr n length = NotEnoughElements <| sprintf "Cannot skip %i elements of the input collection, since it only contains %i elements" n length
  let splitIntoErr n = NegativeInput <| sprintf "Cannot splitInto %i chunks; the count must be positive" n
  let subErr startIndex count length = IndexOutOfRange <| sprintf "The input array does not have sufficient elements for the bounds specified when slicing.  StartIndex: %i; Count: %i; Array length: %i" startIndex count length
  let tailErr = SeqIsEmpty "Cannot get the tail (all but the first element) of an empty sequence"
  let takeErr n length = NotEnoughElements <| sprintf "Cannot take %i elements of the input collection, since it only contains %i elements" n length
  let lazyTakeErr n = NotEnoughElements <| sprintf "Cannot take %i elements of the input collection, since it does not contain enough elements" n
  let unconsErr = SeqIsEmpty "Cannot uncons (separate the first element) of an empty sequence"
  let windowedErr n = NegativeInput <| sprintf "Cannot split collection into windows of size %i; the size must be positive" n
  let zipErr length1 length2 = differingLengthsErr "zip" length1 length2
  let zip3Err length1 length2 length3 = differingLengthsErr3 "zip3" length1 length2 length3