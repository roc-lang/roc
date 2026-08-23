# Regression test: an unbalanced paren swallows the `main!` declaration below
# it, so the headerless file no longer looks like a default app. `roc run` must
# still report the syntax error rather than a misleading "expected app header".
foo = |n| {
    ((n)
}

main! = |_args| {
    echo!(foo(1).to_str())
    Ok({})
}
