module SafetyFirst.Specs.MatchSpec

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst

[<Test>]
let ``matches sequences of up to 5`` () = 
  test 
    <@
      match [1 .. 10] with
      | SeqFiveOrMore (one, two, three, four, five, tail) ->
        one = 1 && two = 2 && three = 3 && four = 4 && five = 5 && Seq.toList tail = [6 .. 10]
      | SeqOneOrMore (one, tail) -> false
      | _ -> false   

      &&

      match [1 .. 4] with
      | SeqFiveOrMore (one, two, three, four, five, tail) -> false
      | SeqFourOrMore (one, two, three, four, tail) ->
        one = 1 && two = 2 && three = 3 && four = 4 && Seq.isEmpty tail
      | SeqOneOrMore (one, tail) -> false
      | _ -> false   

      &&

      match [1 .. 3] with
      | SeqFiveOrMore (one, two, three, four, five, tail) -> false
      | SeqThreeOrMore (one, two, three, tail) ->
        one = 1 && two = 2 && three = 3 && Seq.isEmpty tail
      | SeqOneOrMore (one, tail) -> false
      | _ -> false   

      &&

      match [1 .. 2] with
      | SeqFiveOrMore (one, two, three, four, five, tail) -> false
      | SeqTwoOrMore (one, two, tail) ->
        one = 1 && two = 2 && Seq.isEmpty tail
      | SeqOneOrMore (one, tail) -> false
      | _ -> false   

      &&

      match [1 .. 1] with
      | SeqFiveOrMore (one, two, three, four, five, tail) -> false
      | SeqOneOrMore (one, tail) ->
        one = 1 && Seq.isEmpty tail
      | _ -> false   

      &&

      match [] with
      | SeqFiveOrMore (one, two, three, four, five, tail) -> false
      | SeqOneOrMore (one, tail) -> false
      | _ -> true
    @>

