# Regression test for https://github.com/roc-lang/roc/issues/9588
#
# `roc test/echo/issue_9588.roc --no-cache` used to panic in monotype lowering:
#   postcheck invariant violated: checked dispatch result type conflicted
#   with an existing Monotype constraint   (src/postcheck/monotype/lower.zig)
#
# Trigger: a statically-dispatched method returning Try(_, [Tag, ..]) with an
# open error union, whose result is propagated into main!'s platform-fixed
# signature (here via `?`, but a re-raising `match` crashed identically).
# Fixed by #9617 (widen open-row Monotypes on late row evidence).
Foo :: { x : U64 }.{
    bar : Foo -> Try(I32, [Baz, ..])
    bar = |{ x: _ }| Ok(0)
}

main! = |_args| {
    foo : Foo
    foo = { x: 0 }
    _n = foo.bar()?
    Ok({})
}
