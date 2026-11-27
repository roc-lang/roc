V1 := []

f! : V1, U64 => U64
f! = |x, n| {
    echo!("e")
    match x {
        _ => 0
    }
}

main! = |_args| Ok({})

# exhaustive-model case
# ctx: 'match_eff'
# type: ('NR', 'V1')
# nominals: {'V1': ('U', (), False, 'V1')}
# branches: [([('W',)], False)]
# locs: {(7, 9): (0, 0)}
# expect: 'known-empty_base'
# issue: 'https://github.com/roc-lang/roc/issues/13290'
