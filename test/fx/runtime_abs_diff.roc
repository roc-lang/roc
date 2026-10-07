app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# abs_diff, driven by a runtime value so the operation reaches the backends
# instead of being folded at compile time. The float operands and their
# difference have fractional parts, so the result depends on subtracting the
# floats themselves rather than their whole parts. The integer operands are the
# two ends of their type's range, whose difference fits only the unsigned
# result type.

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    wide : F64
    wide = n.to_f64() * 0.5

    narrow : F32
    narrow = n.to_f32() * 0.5

    Stdout.line!("f64: ${wide.abs_diff(0.25).to_str()} ${F64.abs_diff(0.25, wide).to_str()}")
    Stdout.line!("f32: ${narrow.abs_diff(0.25).to_str()} ${F32.abs_diff(0.25, narrow).to_str()}")

    offset = n.to_i128() - 3

    lowest : I128
    lowest = I128.lowest + offset

    highest : I128
    highest = I128.highest - offset

    Stdout.line!("i128: ${lowest.abs_diff(highest).to_str()} ${highest.abs_diff(lowest).to_str()}")

    small : I8
    small = I8.lowest + offset.to_i8_wrap()

    big : I8
    big = I8.highest - offset.to_i8_wrap()

    Stdout.line!("i8: ${small.abs_diff(big).to_str()} ${big.abs_diff(small).to_str()}")
}
