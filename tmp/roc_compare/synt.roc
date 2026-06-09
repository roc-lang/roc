app [main!] { pf: platform "./platform/main.roc" }
import pf.Plant exposing [Plant]

call_param! : (() => Plant) => Plant
call_param! = |f!| f!()

Wrap(a) := { run! : () => a }.{
    go! : Wrap(a) => a
    go! = |w| match w { { run! } => run!() }
}

main! : () => List(Plant)
main! = || [call_param!(|| Plant.random!(0))]
