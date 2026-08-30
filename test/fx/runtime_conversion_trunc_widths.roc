app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Float-to-integer conversions that discard the fractional part, driven by a
# runtime value so they reach the backends instead of being folded.

f64_to_i64 : F64 -> I64
f64_to_i64 = |x| x.to_i64_wrap()

f64_to_u8 : F64 -> U8
f64_to_u8 = |x| x.to_u8_wrap()

f32_to_i32 : F32 -> I32
f32_to_i32 = |x| x.to_i32_wrap()

main! = || {
    n = match U8.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    x = n.to_f64() + 0.75

    # The fractional part is discarded toward zero in both directions.
    Stdout.line!("toward zero: ${f64_to_i64(x).to_str()} ${f64_to_i64(0.0 - x).to_str()}")
    Stdout.line!("narrow: ${f64_to_u8(x).to_str()} ${f32_to_i32(x.to_f32_wrap()).to_str()}")
}
