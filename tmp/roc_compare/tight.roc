app [main!] { pf: platform "./platform/main.roc" }

import pf.Plant exposing [Plant]

starting_plants! : () => List(Plant)
starting_plants! = || {
    var $list = List.with_capacity(15)
    var $i = 0
    while ($i <= 14) {
        $list = $list.append(Plant.random!($i * 12))
        $i = $i + 1
    }
    $list
}

main! : () => List(Plant)
main! = || starting_plants!()
