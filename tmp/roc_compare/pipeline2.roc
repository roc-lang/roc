app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

starting_plants! : () => List(Plant)
starting_plants! = || lo_to_hi().map!(|i| Plant.random!(i * 12)).to_list()

lo_to_hi : () -> Iter(I32)
lo_to_hi = || {
    lo : I32
    lo = 0
    hi : I32
    hi = 14
    lo.to(hi)
}

main! : () => List(Plant)
main! = || starting_plants!()
