Rec1 := [Leaf1, Node1(Rec1, U64)]

f : Rec1, U64 -> U64
f = |x, n| {
    match x {
        Node1(Rec1.Leaf1, 1) => 0
        _ => 1
        _ => 2
        _ | _ => 3
    }
}

main! = |_args| Ok({})

# exhaustive-model case
# ctx: 'match_pure'
# type: ('NR', 'Rec1')
# nominals: {'Rec1': ('U', (('Leaf1', ()), ('Node1', (('NR', 'Rec1'), ('N', 'U64')))), False, 'Rec1')}
# branches: [([('C', 'Node1', (('C', 'Leaf1', ()), ('Lit', 1)))], False), ([('W',)], False), ([('W',)], False), ([('W',), ('W',)], False)]
# locs: {(6, 9): (0, 0), (7, 9): (1, 0), (8, 9): (2, 0), (9, 9): (3, 0), (9, 13): (3, 1)}
# expect: 'ok'
