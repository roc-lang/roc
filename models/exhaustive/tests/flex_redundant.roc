mk : U64 -> [A, B]
mk = |_| crash "no"

main! = |args| {
    n = List.len(args)
    r = match mk(n) {
        A | _ => 0
        _ => 1
    }
    echo!(r.to_str())
    echo!("done")
    Ok({})
}

# exhaustive-model case
# ctx: 'main_call_match'
# type: ('U', (('A', ()), ('B', ())), 'flex', None)
# nominals: {}
# branches: [([('C', 'A', ()), ('W',)], False), ([('W',)], False)]
# locs: {(7, 9): (0, 0), (7, 13): (0, 1), (8, 9): (1, 0)}
# expect: 'known-flex'
# issue: 'https://github.com/roc-lang/roc/issues/13294'
