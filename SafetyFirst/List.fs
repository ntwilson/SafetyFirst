module SafetyFirst.List

open SafetyFirst.ErrorTypes

let map2Safe f xs ys = 
  if List.length xs = List.length ys 
  then Ok (List.map2 f xs ys)
  else Error map2Err
