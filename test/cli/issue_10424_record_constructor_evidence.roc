D : {}

S : { d : D }

r = {}

s : S
s = { d: r }

consume : S -> {}
consume = |_| {}

main! = |_| {
    _ = consume({ ..s, d: r })
    Ok({})
}
