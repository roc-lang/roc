# Regression fixture for https://github.com/roc-lang/roc/issues/9519 —
# "Monotype function template was assigned two lifted function ids". The two
# expects reach the same generic closure specialization through two call
# sites; flaky identity once assigned it two lifted function ids.
main! = |_args| Ok({})

Foo(a) := [Bar(a), Baz]

combine : Foo(a), Foo(b), (a, b -> c) -> Foo(c)
combine = |ma, mb, f|
    match (ma, mb) {
        (Bar(a), Bar(b)) => Bar(f(a, b))
        _ => Baz
    }

expect combine(Baz, Baz, |a, b| a + b) == Baz
expect combine(Bar(1), Bar(2), |a, b| a + b) == Bar(3)
