app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    zero : U8
    zero = 0
    zero_hex = u4_to_hex(zero)
    Stdout.line!(zero_hex)
}

u4_to_hex : U8 -> Str
u4_to_hex = |n| {
    match n {
        _ => "0"
    }
}
