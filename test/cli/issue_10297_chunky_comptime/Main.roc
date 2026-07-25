# Regression test for https://github.com/roc-lang/roc/issues/10297.

foo : U64, List(U8) -> List(U8)
foo = |n, acc|
    if n == 0 {
        acc
    } else {
        foo(n - 1, acc.append(1))
    }

main! = |args| {
    # The runtime-dependent args.len() keeps bar as its own hoisted root, so
    # checking must evaluate and store the million-element list itself.
    bar = foo(1000000, [])
    Ok(bar.len() + args.len())
}
