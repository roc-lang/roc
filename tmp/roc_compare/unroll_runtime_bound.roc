app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

main! : () => List(Plant)
main! = || {
    bound = Plant.random!(0).x
    var $sum = 0
    var $i = 0
    while $i <= bound {
        $sum = $sum + Plant.random!($i * 12).x
        $i = $i + 1
    }
    [Plant.random!($sum)]
}
