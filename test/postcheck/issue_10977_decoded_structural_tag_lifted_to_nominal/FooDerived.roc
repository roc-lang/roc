FooDerived(a) := [Baz, Qux(a)].{
    parser_for : _

    fizz : FooDerived(a), a -> a
    fizz = |foo, fallback|
        match foo {
            Qux(a) => a
            Baz => fallback
        }
}
