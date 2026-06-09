app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]
main! : () => List(Plant)
main! = || {
    var $sum = 0
    for i in 0.I32.to(14) {
        $sum = $sum + Plant.random!(i * 12).x
    }
    [Plant.random!($sum)]
}
