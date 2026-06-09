app [main!] { pf: platform "./platform/main.roc" }

import pf.Plant exposing [Plant]

iter_map! : Iter(a), (a => b) => Iter(b)
iter_map! = |iterator, transform!| {
    var $list = []
    for item in iterator {
        $list = $list.append(transform!(item))
    }
    List.iter($list)
}

main! : () => List(Plant)
main! = || {
    mapped = iter_map!(0.to(14), |i| Plant.random!(i * 12))
    collect_tmp(mapped)
}

collect_tmp : Iter(item) -> List(item)
collect_tmp = |iterator| {
    var $list = []
    for item in iterator {
        $list = $list.append(item)
    }
    $list
}
