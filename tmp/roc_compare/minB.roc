app [main!] { pf: platform "./platform/main.roc" }

import pf.Plant exposing [Plant]

iter_map! : Iter(a), (a => b) => Iter(b)
iter_map! = |iterator, transform!| {
    var $list = []
    for item in iterator { $list = $list.append(transform!(item)) }
    List.iter($list)
}

collect : Iter(item) -> List(item)
collect = |iterator| {
    var $list = []
    for item in iterator { $list = $list.append(item) }
    $list
}

main! : () => List(Plant)
main! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    collect(iter_map!(lo.to(hi), |i| Plant.random!(i * 12)))
}
