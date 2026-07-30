Rle(a) : [One(a), Many(U64, a)]

x : List(Rle(Str))
x = [One("x")]

test = x == [One("a")]

main! = |_| {
    Ok({})
}
