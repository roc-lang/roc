f! : List(U64), U64 => U64
f! = |x, n| {
    echo!("e")
    match x {
        [0, .. as rest1] => 0
        [_, .. as rest3, 0] => 1
        _ | [0, 2, ..] if n > 2 => 2
        [] as w6 => 3
        [_] => 4
        [v8, .., _] => 5
    }
}

main! = |_args| Ok({})

# exhaustive-model case
# ctx: 'match_eff'
# type: ('L', ('N', 'U64'))
# nominals: {}
# branches: [([('Lst', (('Lit', 0),), ())], False), ([('Lst', (('W',),), (('Lit', 0),))], False), ([('W',), ('Lst', (('Lit', 0), ('Lit', 2)), ())], True), ([('Lst', (), None)], False), ([('Lst', (('W',),), None)], False), ([('Lst', (('W',),), (('W',),))], False)]
# locs: {(5, 9): (0, 0), (6, 9): (1, 0), (7, 9): (2, 0), (7, 13): (2, 1), (8, 9): (3, 0), (9, 9): (4, 0), (10, 9): (5, 0)}
# expect: 'ok'
