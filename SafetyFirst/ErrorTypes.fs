namespace SafetyFirst 

type SeqIsEmpty = SeqIsEmpty of string
type NotEnoughElements = NotEnoughElements of string
type NoMatchingElement = NoMatchingElement of string
type DifferingLengths = DifferingLengths of string
type NegativeInput = NegativeInput of string
type WrongNumberOfElements = WrongNumberOfElements of string
type IndexOutOfRange = IndexOutOfRange of string

module internal ErrorTypes = 
  let inline avgErr () = SeqIsEmpty "Cannot get the average value of an empty sequence"
  let chunkErr = NegativeInput "Cannot chunkBySize with a negative size"
  let moreThanOneErr = WrongNumberOfElements "Sequence was expected to have a single element, but has more than one"
  let lessThanOneErr = WrongNumberOfElements "Sequence was expected to have a single element, but was empty"
  let exists2Err length1 length2 = DifferingLengths <| sprintf "Cannot call exists2 with two sequences that have different lengths (%i and %i)" length1 length2
  let findErr = NoMatchingElement "Cannot find an element in the input collection matching the input predicate"
  let fold2Err length1 length2 = DifferingLengths <| sprintf "Cannot call fold2 with two sequences that have different lengths (%i and %i)" length1 length2
  let headErr = SeqIsEmpty "Cannot get the first element (head) of an empty sequence"
  let indexTooLargeErr index = IndexOutOfRange <| sprintf "Cannot access the element at index %i in the input sequence because the sequence doesn't have enough elements" index
  let indexNegativeErr index = IndexOutOfRange <| sprintf "Cannot access the element at a negative index value (%i used)" index
  let lastErr = SeqIsEmpty "Cannot retrieve the last element of an empty sequence"
  let map2Err length1 length2 = DifferingLengths <| sprintf "Cannot call map2 with two sequences that have different lengths (%i and %i)" length1 length2
  let maxErr = SeqIsEmpty "Cannot produce a maximum element from an empty sequence"
  let minErr = SeqIsEmpty "Cannot produce a minimum element from an empty sequence"
  let pickErr = NoMatchingElement "Cannot find an element in the input collection matching the input predicate"
  let reduceErr = SeqIsEmpty "Cannot reduce an empty sequence"
  let lazySkipErr n = NotEnoughElements <| sprintf "Cannot skip %i elements of the input collection, since it does not contain enough elements" n
  let skipErr n length = NotEnoughElements <| sprintf "Cannot skip %i elements of the input collection, since it only contains %i elements" n length
  let splitIntoErr n = NegativeInput <| sprintf "Cannot splitInto %i chunks; the count must be positive" n
  let inline sumErr () = SeqIsEmpty "Cannot get the sum of an empty sequence"
  let tailErr = SeqIsEmpty "Cannot get the tail (all but the first element) of an empty sequence"
  let takeErr n length = NotEnoughElements <| sprintf "Cannot take %i elements of the input collection, since it only contains %i elements" n length
  let lazyTakeErr n = NotEnoughElements <| sprintf "Cannot take %i elements of the input collection, since it does not contain enough elements" n
  let unconsErr = SeqIsEmpty "Cannot uncons (separate the first element) of an empty sequence"
  let windowedErr n = NegativeInput <| sprintf "Cannot split collection into windows of size %i; the size must be positive" n
  let zipErr length1 length2 = DifferingLengths <| sprintf "Cannot zip two sequences that have differing lengths (%i and %i)" length1 length2
  