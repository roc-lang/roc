app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Conversions that wrap an out-of-range value into the destination range, driven
# by a runtime value so they reach the backends instead of being folded.

i8_to_u16 : I8 -> U16
i8_to_u16 = |n| n.to_u16_wrap()

u16_to_i8 : U16 -> I8
u16_to_i8 = |n| n.to_i8_wrap()

u16_to_u8 : U16 -> U8
u16_to_u8 = |n| n.to_u8_wrap()

main! = || {
    n = match U16.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    small = n.to_u8_wrap().to_i8_wrap()

    # In range, wrapping is the identity.
    Stdout.line!("small: ${u16_to_i8(n).to_str()} ${u16_to_u8(n).to_str()}")

    # A negative source sign-extends before it is reinterpreted.
    Stdout.line!("negative: ${i8_to_u16(0 - small).to_str()}")

    # Past the destination range the value reduces modulo that range.
    big = n * 100
    Stdout.line!("big: ${u16_to_u8(big).to_str()} ${u16_to_i8(big).to_str()}")
}
