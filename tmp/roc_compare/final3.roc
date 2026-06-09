app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

starting_plants! : () => List(Plant)
starting_plants! = || {
    0.I32.to(14).stream().map(|i| Plant.random!(i * 12)).collect!()
}

main! : () => List(Plant)
main! = || starting_plants!()
