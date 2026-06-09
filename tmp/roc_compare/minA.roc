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

seeds : List(I32)
seeds = [0, 12, 24]

main! : () => List(Plant)
main! = || collect(iter_map!(List.iter(seeds), |i| Plant.random!(i)))
