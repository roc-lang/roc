app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

main! : () => List(Plant)
main! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    var $list = []
    for i in lo.to(hi) { $list = $list.append(Plant.random!(i * 12)) }
    $list
}
