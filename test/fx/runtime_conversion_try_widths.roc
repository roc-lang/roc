app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Conversions that return `Err` when the value does not fit the destination,
# driven by a runtime value so they reach the backends instead of being folded.

u16_to_i8 : U16 -> Str
u16_to_i8 = |n|
    match n.to_i8_try() {
        Ok(v) => v.to_str()
        Err(_) => "out of range"
    }

u16_to_u8 : U16 -> Str
u16_to_u8 = |n|
    match n.to_u8_try() {
        Ok(v) => v.to_str()
        Err(_) => "out of range"
    }

f64_to_i8 : F64 -> Str
f64_to_i8 = |x|
    match x.to_i8_try() {
        Ok(v) => v.to_str()
        Err(_) => "out of range"
    }

main! = || {
    n = match U16.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    Stdout.line!("fits: ${u16_to_i8(n)} ${u16_to_u8(n)}")
    Stdout.line!("past i8: ${u16_to_i8(n * 50)} ${u16_to_u8(n * 50)}")
    Stdout.line!("past u8: ${u16_to_u8(n * 1000)}")

    x = n.to_f64()
    Stdout.line!("from f64: ${f64_to_i8(x + 1.5)} ${f64_to_i8(x * 100.0)}")
}
