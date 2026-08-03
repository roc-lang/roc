# Companion module for ../issue_9519_two_lifted_ids.roc: the same generic
# function reached through a module boundary. Issue #9519's flaky identity
# depended on file-split order; both arrangements must keep one lifted
# function id per specialization.
Combine :: {}.{
    Foo(a) := [Bar(a), Baz].{
        is_eq : _
    }

    combine : Foo(a), Foo(b), (a, b -> c) -> Foo(c)
    combine = |ma, mb, f|
        match (ma, mb) {
            (Bar(a), Bar(b)) => Bar(f(a, b))
            _ => Baz
        }
}
