module SafetyFirst.Specs.MapSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``Errors when constructing a Map that contains duplicate keys`` () = 
  test 
    <@
      match  
        Map.ofSeq' 
          [
            (1, "this")
            (2, "map")
            (3, "has")
            (2, "duplicate")
            (8, "keys")
          ]
        with
      | Error _ -> true
      | _ -> false        

      &&

      Map.ofSeq'
        [
          (1, "this")
          (2, "map")
          (4, "is")
          (8, "fine")
        ]
        =
        Ok (Map.ofSeq
          [
            (1, "this")
            (2, "map")
            (4, "is")
            (8, "fine")
          ])
    @>


