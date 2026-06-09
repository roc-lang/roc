app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

main! : () => List(Plant)
main! = || {
    var $sum = 0
    var $i = 0
    while $i <= 14 {
        p = Plant.random!($i * 12)
        $sum = $sum + p.x
        $i = $i + 1
    }
    [Plant.random!($sum)]
}
