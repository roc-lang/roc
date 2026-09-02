import Foo

Bar := [].{
    Fuzz : { alpha : Str, beta : [Qux(Str), Baz] }

    frob : Fuzz -> Str
    frob = |fuzz| "${fuzz.alpha}: ${Foo.fizz(fuzz.beta, "buzz")}"
}
