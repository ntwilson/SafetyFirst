module SafetyFirst.Specs.Generators

open FsCheck
open SafetyFirst.Numbers

type Numbers =
  static member private gen_NaturalInt = gen {
    let! x = Arb.generate<int>
    return NaturalInt.assume (abs x)
  }
  static member NaturalIntGen() = { new Arbitrary<NaturalInt>() with override this.Generator = Numbers.gen_NaturalInt }

  static member private gen_PositiveInt = gen {
    let! t = Numbers.gen_NaturalInt
    return t.Increment
  }
  static member PositiveIntGen() = { new Arbitrary<PositiveInt>() with override this.Generator = Numbers.gen_PositiveInt }

  static member private gen_NegativeInt = gen {
    let! p = Numbers.gen_PositiveInt
    return p.Opposite
  }
  static member NegativeIntGen() = { new Arbitrary<NegativeInt>() with override this.Generator = Numbers.gen_NegativeInt }