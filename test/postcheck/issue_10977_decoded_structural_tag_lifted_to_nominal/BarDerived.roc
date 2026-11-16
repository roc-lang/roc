import FooDerived

BarDerived := [].{
    Fuzz : { beta : FooDerived(Str) }

    frob : Fuzz -> Str
    frob = |fuzz| FooDerived.fizz(fuzz.beta, "buzz")
}
