module SafetyFirst.Seq

type SeqIsEmpty = SeqIsEmpty of string
let headSafe xs =
  if Seq.isEmpty xs 
  then Error <| SeqIsEmpty "Cannot get the first element (head) of an empty sequence"
  else Ok <| Seq.head xs

let head' xs = headSafe xs
