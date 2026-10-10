app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# 128-bit integer to float conversions, driven by a runtime value so the
# operations reach the backends instead of being folded at compile time. The
# operands are past U64 range, so the results depend on the full 128-bit value
# rather than its low half.

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    unsigned : U128
    unsigned = n.to_u128() * 100000000000000000000

    signed : I128
    signed = 0 - unsigned.to_i128_wrap()

    Stdout.line!("unsigned: ${unsigned.to_f64().to_str()} ${unsigned.to_f32().to_str()}")
    Stdout.line!("signed: ${signed.to_f64().to_str()} ${signed.to_f32().to_str()}")

    # The largest U128 rounds up to 2^128, which F64 holds and F32 does not.
    # The high bit of its upper half makes a signed reading of the same bits
    # negative.
    highest : U128
    highest = U128.highest - (n.to_u128() - 3)

    Stdout.line!("highest: ${highest.to_f64().to_str()} ${highest.to_f32().to_str()}")
}
