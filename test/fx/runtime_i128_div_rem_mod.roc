app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# 128-bit division, remainder and modulo, driven by a runtime value so the
# operations reach the backends instead of being folded at compile time. The
# operands are past U64 range, so the results depend on the full 128-bit
# arithmetic rather than its low half.

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    unsigned : U128
    unsigned = n.to_u128() * 100000000000000000000

    signed : I128
    signed = unsigned.to_i128_wrap()

    Stdout.line!("unsigned: ${(unsigned // 7).to_str()} ${(unsigned % 7).to_str()} ${unsigned.mod_by(7).to_str()}")
    Stdout.line!("signed: ${(signed // 7).to_str()} ${(signed % 7).to_str()} ${signed.mod_by(7).to_str()}")

    # Remainder keeps the sign of the dividend; modulo keeps the sign of the
    # divisor, so the two disagree once the dividend is negative.
    neg = 0 - signed
    Stdout.line!("negative: ${(neg // 7).to_str()} ${(neg % 7).to_str()} ${neg.mod_by(7).to_str()}")
}
