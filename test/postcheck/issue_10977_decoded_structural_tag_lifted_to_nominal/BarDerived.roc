import FooDerived

BarDerived := [].{
    Fuzz : { alpha : Str, beta : [Qux(Str), Baz] }

    frob : Fuzz -> Str
    frob = |fuzz| "${fuzz.alpha}: ${FooDerived.fizz(fuzz.beta, "buzz")}"
}
