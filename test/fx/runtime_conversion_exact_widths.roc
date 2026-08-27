app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Conversions whose destination holds every source value, driven by a runtime
# value so they reach the backends instead of being folded at compile time.

u8_to_i16 : U8 -> I16
u8_to_i16 = |n| n.to_i16()

u8_to_u16 : U8 -> U16
u8_to_u16 = |n| n.to_u16()

u8_to_f64 : U8 -> F64
u8_to_f64 = |n| n.to_f64()

u8_to_dec : U8 -> Dec
u8_to_dec = |n| n.to_dec()

i8_to_i16 : I8 -> I16
i8_to_i16 = |n| n.to_i16()

main! = || {
    n = match U8.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    neg = 0 - n.to_i16()

    Stdout.line!("int: ${u8_to_i16(n).to_str()} ${u8_to_u16(n).to_str()}")
    Stdout.line!("frac: ${u8_to_f64(n).to_str()} ${u8_to_dec(n).to_str()}")
    Stdout.line!("signed: ${i8_to_i16(neg.to_i8_wrap()).to_str()}")
}
