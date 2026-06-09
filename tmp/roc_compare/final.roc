app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

starting_plants! : () => List(Plant)
starting_plants! = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    lo.to(hi).map!(|i| Plant.random!(i * 12)).collect!()
}

main! : () => List(Plant)
main! = || starting_plants!()
