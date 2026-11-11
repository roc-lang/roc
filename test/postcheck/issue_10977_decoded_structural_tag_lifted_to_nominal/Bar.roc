import Foo

Bar := [].{
    Fuzz : { alpha : Str, beta : Foo(Str) }

    frob : Fuzz -> Str
    frob = |fuzz| "${fuzz.alpha}: ${Foo.fizz(fuzz.beta, "buzz")}"
}
