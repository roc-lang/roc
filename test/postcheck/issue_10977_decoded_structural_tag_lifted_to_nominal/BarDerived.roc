import FooDerived

BarDerived := [].{
    Fuzz : { beta : [Qux(Str), Baz] }

    frob : Fuzz -> Str
    frob = |fuzz| FooDerived.fizz(fuzz.beta, "buzz")
}
