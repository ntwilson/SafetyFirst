namespace SafetyFirst

open System.Collections.Generic

open ResultDotNet.FSharp

/// <summary>
/// A sequence that is constrained to have at least one element.
/// </summary>
type NonEmpty<'a, 'b when 'a :> 'b seq> = 
  private
    | NonEmpty of 'a

  interface IEnumerable<'b> with
    member this.GetEnumerator () = 
      let (NonEmpty xs) = this
      in (xs :> _ seq).GetEnumerator ()

    member this.GetEnumerator () : System.Collections.IEnumerator = 
      let (NonEmpty xs) = this
      in upcast ((xs :> _ seq).GetEnumerator ())
  
[<AutoOpen>]
module NonEmptySeqMatcher = 
  let (|Empty|NotEmpty|) (xs:#seq<'a>) = 
    if Seq.isEmpty xs
    then Empty
    else NotEmpty (NonEmpty xs)

  let (|NonEmpty|) (NonEmpty xs) = xs

