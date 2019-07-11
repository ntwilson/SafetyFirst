namespace SafetyFirst.FSharpxCopy.Collections

//THIS FILE (partially) LIFTED STRAIGHT FROM https://github.com/fsprojects/FSharpx.Collections/blob/311c74433fd616611554f38f182bf860c24b91ee/src/FSharpx.Collections/LazyList.fs
//FSharpx.Collections is forcing the use of .NET Framework instead of .NET Standard
//once FSharpx.Collections v2.0 is released, we can remove much of the contents of this file and reference FSharpx.Collections instead

open System
open System.Collections.Generic
open SafetyFirst.Numbers

#nowarn "21" // recursive initialization
#nowarn "40" // recursive initialization

exception UndefinedException

[<NoEquality; NoComparison>]
type  LazyList<'T> =
    { mutable status : LazyCellStatus< 'T > }
    
    member x.Value = 
        match x.status with 
        | LazyCellStatus.Value v -> v
        | _ -> 
            lock x (fun () -> 
                match x.status with 
                | LazyCellStatus.Delayed f -> 
                    x.status <- Exception UndefinedException; 
                    try 
                        let res = f () 
                        x.status <- LazyCellStatus.Value res; 
                        res 
                    with e -> 
                        x.status <- LazyCellStatus.Exception(e); 
                        reraise()
                | LazyCellStatus.Value v -> v
                | LazyCellStatus.Exception e -> raise e)
    
    static member inline force (x: LazyList<'T>) = x.Value
    static member inline getCell (x : LazyList<'T>) = LazyList.force x

    member this.IsEmpty =
      match LazyList.getCell this with
      | CellCons _ -> false
      | CellEmpty -> true
      
    member this.TryHead = 
      match LazyList.getCell this with
      | CellCons(a,_) -> Some a
      | CellEmpty -> None

    member this.Length() = 
        let rec lengthAux n s = 
          match LazyList.getCell s with
          | CellEmpty -> n
          | CellCons(_,b) -> lengthAux (n+1) b

        lengthAux 0 this

    member this.TryTail = 
      match LazyList.getCell this with
      | CellCons(_,b) -> Some b
      | CellEmpty -> None

    member this.TryUncons = match LazyList.force this with CellCons (a,b) -> Some(a,b) | CellEmpty -> None

    member s.GetEnumeratorImpl() = 
        let getCell (x : LazyList<'T>) = x.Value
        let toSeq s = Seq.unfold (fun ll -> match getCell ll with CellEmpty -> None | CellCons(a,b) -> Some(a,b)) s 
        (toSeq s).GetEnumerator()
            
    interface IEnumerable<'T> with
        member s.GetEnumerator() = s.GetEnumeratorImpl()

    interface System.Collections.IEnumerable with
        override s.GetEnumerator() = (s.GetEnumeratorImpl() :> System.Collections.IEnumerator)


and 
    [<NoEquality; NoComparison>]
     LazyCellStatus<'T> =
    | Delayed of (unit -> LazyListCell<'T> )
    | Value of LazyListCell<'T> 
    | Exception of System.Exception


and 
    [<NoEquality; NoComparison>]
     LazyListCell<'T> = 
    | CellCons of 'T * LazyList<'T> 
    | CellEmpty

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module  LazyList = 

    let lzy f = { status = Delayed f }
    let force (x: LazyList<'T>) = x.Value

    let notlazy v = { status = Value v }
    
    type EmptyValue<'T>() = 
        static let value : LazyList<'T> = notlazy CellEmpty
        static member Value : LazyList<'T> = value
        
    [<NoEquality; NoComparison>]
    type LazyItem<'T> = Cons of 'T * LazyList<'T> | Empty
    type 'T item = 'T LazyItem

    let getCell (x : LazyList<'T>) = force x 
    let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value
    let consc x l = CellCons(x,l)
    let cons x l = lzy(fun () -> (consc x l))
    let consDelayed x l = lzy(fun () -> (consc x (lzy(fun () ->  (force (l()))))))

    let uncons (s : LazyList<'T>) = s.TryUncons

    let rec unfold f z = 
      lzy(fun () -> 
          match f z with
          | None       -> CellEmpty
          | Some (x,z) -> CellCons (x,unfold f z))

    let rec append l1  l2 = lzy(fun () ->  (appendc l1 l2))
    and appendc l1 l2 =
      match getCell l1 with
      | CellEmpty -> force l2
      | CellCons(a,b) -> consc a (append b l2)

    let delayed f = lzy(fun () ->  (getCell (f())))

    let initInfinitely (initializer:NaturalInt -> 'a) = 
      let rec s i () = consc (initializer i) (lzy (s i.Increment.AsNatural))
      lzy (s NaturalInt.zero)

    let initN count (initializer:NaturalInt -> 'a) =
      let rec s i () = if i = count then CellEmpty else consc (initializer i) (lzy (s i.Increment.AsNatural))
      lzy (s NaturalInt.zero)

    let replicateInfinitely initial = 
      let rec s () = consc initial (lzy s)
      lzy s

    let replicateN count initial = 
      let rec s n () = match n with | ZeroNatural -> CellEmpty | PositiveNatural count -> consc initial (lzy (s count.Decrement))
      lzy (s count)
      
    let rec map f s = 
      lzy(fun () ->  
        match getCell s with
        | CellEmpty -> CellEmpty
        | CellCons(a,b) -> consc (f a) (map f b))

    let rec map2 f s1 s2 =  
      lzy(fun () -> 
        match getCell s1, getCell s2  with
        | CellCons(a1,b1),CellCons(a2,b2) -> consc (f a1 a2) (map2 f b1 b2)
        | _ -> CellEmpty)

    let rec zip s1 s2 = 
      lzy(fun () -> 
        match getCell s1, getCell s2  with
        | CellCons(a1,b1),CellCons(a2,b2) -> consc (a1,a2) (zip b1 b2)
        | _ -> CellEmpty)

    let rec concat s1 = 
      lzy(fun () -> 
        match getCell s1 with
        | CellCons(a,b) -> appendc a (concat b)
        | CellEmpty -> CellEmpty)
      
    let rec filter p s1= lzy(fun () ->  filterc p s1)
    and filterc p s1 =
        match getCell s1 with
        | CellCons(a,b) -> if p a then consc a (filter p b) else filterc p b
        | CellEmpty -> CellEmpty
      
    let rec tryFind p s1 =
        match getCell s1 with
        | CellCons(a,b) -> if p a then Some a else tryFind p b
        | CellEmpty -> None

    let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException("An index satisfying the predicate was not found in the collection"))

    let find p s1 =
        match tryFind p s1 with
        | Some a -> a
        | None   -> indexNotFound()

    let rec scan f acc s1 = 
      lzy(fun () -> 
        match getCell s1 with
        | CellCons(a,b) -> let acc' = f acc a in consc acc (scan f acc' b)
        | CellEmpty -> consc acc empty)

    let head (s : LazyList<'T>) = s.TryHead

    let tail (s : LazyList<'T>) = s.TryTail

    let isEmpty (s : LazyList<'T>) = s.IsEmpty

    let rec take n s = 
        if n < 0 then None
        elif n = 0 then Some empty
        else
            match getCell s with
            | CellCons(a,s) -> Some (consDelayed a ( fun () -> match (take (n-1) s) with Some x -> x | None -> empty ) )
            | CellEmpty -> None

    let rec skipc n s =
      if n <= 0 then Some s
      else  
        match getCell s with
        | CellCons(_,s) -> match (skipc (n-1) s) with Some x -> Some x | None -> None
        | CellEmpty -> None

    let rec skip n s = skipc n s

    let fold f s l =
        let rec loop s l cont =
            match  getCell l with
            | CellEmpty -> cont s
            | CellCons(x,xs) ->
                let s = f s x
                loop s xs (fun s -> cont s)
        loop s l id

    let mapAccum f s l =
        let rec loop s l cont =
            match  getCell l with
            | CellEmpty -> cont (s, empty)
            | CellCons(x,xs) ->
                let s, y = f s x
                loop s xs (fun (s,ys) -> cont (s, cons y ys))
        loop s l id

    let rec ofList l = 
      lzy(fun () -> 
        match l with [] -> CellEmpty | h :: t -> consc h (ofList t))
      
    let toList s = 
      let rec loop s acc = 
          match getCell s with
          | CellEmpty -> List.rev acc
          | CellCons(h,t) -> loop t (h::acc)
      loop s []
      
    let rec iter f s = 
      match getCell s with
      | CellEmpty -> ()
      | CellCons(h,t) -> f h; iter f t
      
    let rec copyFrom i a = 
      lzy(fun () -> 
        if i >= Array.length a then CellEmpty 
        else consc a.[i] (copyFrom (i+1) a))
      
    let rec copyTo (arr: _[]) s i = 
      match getCell s with
      | CellEmpty -> ()
      | CellCons(a,b) -> arr.[i] <- a; copyTo arr b (i+1)

    let ofArray a = copyFrom 0 a
    let toArray s = Array.ofList (toList s)
      
    let rec lengthAux n s = 
      match getCell s with
      | CellEmpty -> n
      | CellCons(_,b) -> lengthAux (n+1) b

    let length (s : LazyList<'T>) = s.Length()

    let toSeq (s: LazyList<'T>) = (s :> IEnumerable<_>)

    // Note: this doesn't dispose of the IEnumerator if the iteration is not run to the end
    let rec ofFreshIEnumerator (e : IEnumerator<_>) = 
      lzy(fun () -> 
        if e.MoveNext() then 
          consc e.Current (ofFreshIEnumerator e)
        else 
           e.Dispose()
           CellEmpty)
      
    let ofSeq (c : IEnumerable<_>) =
      ofFreshIEnumerator (c.GetEnumerator()) 
      
    let (|Cons|Nil|) l = match getCell l with CellCons(a,b) -> Cons(a,b) | CellEmpty -> Nil


    let rec private revAux r acc =
        match r with
        | Nil -> acc
        | Cons(hd, tl) -> revAux tl (cons hd acc)

    let rev r =
        revAux r empty

    let rec drop (n:NaturalInt) xs =
        match n with
        | PositiveNatural p ->
            match xs with
            | Cons(x, xs') -> drop p.Decrement xs'
            | _ -> EmptyValue<'T>.Value
        | ZeroNatural ->
            xs

