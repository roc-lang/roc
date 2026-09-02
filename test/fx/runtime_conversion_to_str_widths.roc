app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Conversions from every number type to text, driven by a runtime value so they
# reach the backends instead of being folded at compile time.

main! = || {
    n = match U8.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    neg = 0 - n.to_i32()

    Stdout.line!("unsigned: ${n.to_str()} ${n.to_u16().to_str()} ${n.to_u32().to_str()} ${n.to_u64().to_str()} ${n.to_u128().to_str()}")
    Stdout.line!("signed: ${neg.to_i8_wrap().to_str()} ${neg.to_i16_wrap().to_str()} ${neg.to_str()} ${neg.to_i64().to_str()} ${neg.to_i128().to_str()}")
    Stdout.line!("frac: ${n.to_f32().to_str()} ${n.to_f64().to_str()} ${n.to_dec().to_str()}")
}
