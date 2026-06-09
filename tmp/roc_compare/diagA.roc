app [main!] { pf: platform "./pf_i32/main.roc" }

collect : Iter(item) -> List(item)
collect = |iterator| {
    var $list = []
    for item in iterator { $list = $list.append(item) }
    $list
}

main! : () => List(I32)
main! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    collect(lo.to(hi))
}
