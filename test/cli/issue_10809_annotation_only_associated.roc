# Repro for https://github.com/roc-lang/roc/issues/10809: an associated
# declaration with a type annotation and no value names no value either, whether
# it is read as a qualified value or reached through method dispatch.
Foo := [A].{
    bar : U64
    baz : Foo -> U64
}

o = Foo.bar

p = {
    v : Foo
    v = A
    v.baz()
}

main! = |_| Ok({})
