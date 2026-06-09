app [main!] { pf: platform "./platform/main.roc" }

import pf.Plant exposing [Plant]

collect : Iter(item) -> List(item)
collect = |iterator| {
    var $list = []
    for item in iterator {
        $list = $list.append(item)
    }
    $list
}

main! : () => List(Plant)
main! = || {
    p = Plant.random!(0)
    collect(List.iter([p]))
}
