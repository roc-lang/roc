Foo(a) := [Baz, Qux(a)].{
    fizz : Foo(a), a -> a
    fizz = |foo, fallback|
        match foo {
            Qux(a) => a
            Baz => fallback
        }
}
