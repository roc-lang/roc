# expect: fail panic:test-dev
# https://github.com/roc-lang/roc/issues/13359 (open): no errors, but `roc test` panics on a function whose constant
# block performs an effect. Change this to `# expect: pass clean` when the issue is fixed.
f = |_a| {
    echo!("x")
    4
}

main! = |_args| {
    echo!(Str.inspect(f(1)))
    Ok({})
}
