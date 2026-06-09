app [main!] { pf: platform "./platform/main.roc" }

import pf.Plant exposing [Plant]

main! : () => List(Plant)
main! = || {
    p = Plant.random!(0)
    [p]
}
