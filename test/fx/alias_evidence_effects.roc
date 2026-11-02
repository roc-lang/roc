app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

say! = |value| Stdout.line!(value.to_str())

speak! = say!

at_u64! : U64 => {}
at_u64! = speak!

out! = Stdout.line!
out_again! = out!

run! = |f!, x| f!(x)

main! = || {
    at_u64!(1)
    speak!(2.U8)
    run!(speak!, 3.U16)
    run!(at_u64!, 4)
    out_again!("hosted")
    run!(out_again!, "hosted value")
}
