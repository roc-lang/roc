app [main!] { pf: platform "./pf_i32/main.roc" }
gather : Iter(item) -> List(item)
gather = |iterator| {
    var $list = []
    for item in iterator {
        $list = $list.append(item)
    }
    $list
}
main! : () => List(I32)
main! = || gather(0.to(14))
