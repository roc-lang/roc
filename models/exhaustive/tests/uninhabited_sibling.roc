V1 := []

mk : U64 -> Try(V1, I64)
mk = |_| crash "no"

f : U64 -> U64
f = |n| {
    Err(1) = mk(n)
    0
}

main! = |args| {
    _ = f(List.len(args))
    Ok({})
}

# exhaustive-model case
# ctx: 'pure_call_destr'
# type: ('U', (('Ok', (('NR', 'V1'),)), ('Err', (('N', 'I64'),))), False, 'Try')
# nominals: {'V1': ('U', (), False, 'V1')}
# branches: [([('C', 'Err', (('Lit', 1),))], False)]
# locs: {(8, 5): (0, 0)}
# expect: 'known-uninh_sibling'
# issue: 'https://github.com/roc-lang/roc/issues/13289'
