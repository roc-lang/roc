app [main!] { pf: platform "./static-lib-platform/main.roc" }

grow : Iter(U64), U64 -> Iter(U64)
grow = |iter, remaining|
    if remaining == 0 {
        iter
    } else {
        grow(iter.concat(Iter.single(remaining)), remaining - 1)
    }

main! : U64 => Str
main! = |_seed| {
    iter = grow([0.U64].iter(), 2)
    var sum = 0.U64
    for item in iter {
        sum = sum + item
    }
    if sum == 3 { "ok" } else { "bad" }
}
