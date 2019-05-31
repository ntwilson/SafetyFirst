namespace SafetyFirst

open System
open System.Runtime.CompilerServices
open System.Collections.Generic

open SafetyFirst.CSharp

open SafetyFirst.ErrorTypes

[<Extension>]
type IEnumerableExtensions () = 

  /// <summary>
  /// Applies an accumulator function over a sequence.
  /// </summary>
  [<Extension>]
  static member AggregateSafe (xs, f:Func<'a,'a,'a>) = 
    FSeq.reduce' (fun a b -> f.Invoke (a, b)) (fseq xs)
    |> Result.toCs

  /// <summary>
  /// Computes the average of a sequence.
  /// </summary>
  [<Extension>]
  static member AverageSafe (xs:int seq) =
    FSeq.averageBy' float (fseq xs)
    |> Result.toCs

  /// <summary>
  /// Computes the average of a sequence.
  /// </summary>
  [<Extension>]
  static member AverageSafe (xs:float seq) = 
    FSeq.average' (fseq xs)
    |> Result.toCs

  /// <summary>
  /// Computes the average of a sequence of Double values that are obtained by invoking 
  /// a transform function on each element of the input sequence.
  /// </summary>
  [<Extension>]
  static member AverageSafe (xs, transform:Func<_,float>) =
    FSeq.averageBy' (transform.Invoke) (fseq xs)  
    |> Result.toCs

  /// <summary>
  /// Returns the element at a specified index in a sequence.
  /// O(1) for any IList<T> or IReadOnlyList<T>.  
  /// O(n) where n is the <c>index</c> for any other IEnumerable<T>.
  /// </summary>
  [<Extension>]
  static member ElementAtSafe (xs:_ seq, index) = 
    if index < 0
      then Error <| indexNegativeErr index
    else 
      let count = 
        match xs with
        | :? ICollection<obj> as xs -> Some xs.Count 
        | :? IReadOnlyCollection<obj> as xs -> Some xs.Count
        | _ -> None

      match count with
      | Some c when c <= index -> Error <| indexTooLargeErr index c
      | Some c -> 
        match xs with
        | :? IList<_> as xs -> Ok <| xs.[index]  
        | :? IReadOnlyList<_> as xs -> Ok <| xs.[index]
        | _ -> Ok <| Seq.item index xs
      | None -> Seq.item' index xs |> Result.toCs


  /// <summary>
  /// Returns the first element of a sequence.
  /// </summary>
  [<Extension>]
  static member FirstSafe (xs) = Seq.head' xs |> Result.toCs


  /// <summary>
  /// Returns the first element in a sequence that satisfies a specified condition.
  /// </summary>
  [<Extension>]
  static member FirstSafe (xs, predicate:Func<_,_>) = 
    Seq.find' (predicate.Invoke) xs
    |> Result.toCs

  /// <summary>
  /// Returns the last element of a sequence.
  /// </summary>
  [<Extension>]
  static member LastSafe (xs) = FSeq.last' (fseq xs) |> Result.toCs
 

  /// <summary>
  /// Returns the last element in a sequence that satisfies a specified condition.
  /// </summary>
  [<Extension>]
  static member LastSafe (xs, predicate:Func<_,_>) = 
    FSeq.findBack' (predicate.Invoke) (fseq xs)
    |> Result.toCs

  /// <summary>
  /// Returns the maximum value in a sequence of values.
  /// </summary>
  [<Extension>]
  static member MaxSafe (xs:int seq) = FSeq.max' (fseq xs) |> Result.toCs


  /// <summary>
  /// Returns the maximum value in a sequence of values.
  /// </summary>
  [<Extension>]
  static member MaxSafe (xs:float seq) = FSeq.max' (fseq xs) |> Result.toCs


  /// <summary>
  /// Invokes a transform function on each element of a sequence and returns the maximum value.
  /// </summary>
  [<Extension>]
  static member MaxSafe (xs, transform:Func<_,float>) =
    FSeq.maxBy' (transform.Invoke) (fseq xs)  
    |> Result.toCs

  /// <summary>
  /// Invokes a transform function on each element of a sequence and returns the maximum value.
  /// </summary>
  [<Extension>]
  static member MaxSafe (xs, transform:Func<_,int>) =
    FSeq.maxBy' (transform.Invoke) (fseq xs)  
    |> Result.toCs

  /// <summary>
  /// Returns the minimum value in a sequence of values.
  /// </summary>
  [<Extension>]
  static member MinSafe (xs:int seq) = FSeq.min' (fseq xs) |> Result.toCs


  /// <summary>
  /// Returns the minimum value in a sequence of values.
  /// </summary>
  [<Extension>]
  static member MinSafe (xs:float seq) = FSeq.min' (fseq xs) |> Result.toCs


  /// <summary>
  /// Invokes a transform function on each element of a sequence and returns the minimum value.
  /// </summary>
  [<Extension>]
  static member MinSafe (xs, transform:Func<'a,float>) =
    FSeq.minBy' (transform.Invoke) (fseq xs)  
    |> Result.toCs

  /// <summary>
  /// Invokes a transform function on each element of a sequence and returns the minimum value.
  /// </summary>
  [<Extension>]
  static member MinSafe (xs, transform:Func<'a,int>) =
    FSeq.minBy' (transform.Invoke) (fseq xs)  
    |> Result.toCs

  /// <summary>
  /// Returns the only element of a sequence
  /// </summary>
  [<Extension>]
  static member SingleSafe (xs) = Seq.exactlyOne' xs |> Result.toCs


  /// <summary>
  /// Returns the only element of a sequence that satisfies a specified condition.
  /// </summary>
  [<Extension>]
  static member SingleSafe (xs, predicate:Func<_, bool>) = 
    Seq.filter (predicate.Invoke) xs
    |> Seq.exactlyOne'
    |> Result.toCs


  /// <summary>
  /// Applies a specified function to the corresponding elements of two sequences, 
  /// producing a sequence of the results.
  /// Returns a DifferingLengths Error if the sequences have different lengths.
  /// Note:
  ///   This function is not lazy, since it checks the lengths of the two sequences
  ///   before returning anything.
  /// </summary>
  [<Extension>]
  static member ZipSafe (xs:_ seq, ys:_ seq, resultSelector:Func<_,_,_>) =
    let cachedXs = ResizeArray xs
    let cachedYs = ResizeArray ys
    
    if cachedXs.Count <> cachedYs.Count
    then Error <| zipErr cachedXs.Count cachedYs.Count
    else Ok <| Seq.map2 (fun a b -> resultSelector.Invoke (a, b)) cachedXs cachedYs


