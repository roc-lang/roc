app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Float operations that some target has no instruction for, driven by a runtime
# value so they reach the backends instead of being folded at compile time. No
# target has a float remainder instruction, and a baseline x86-64 CPU has none
# for rounding either, so compiled code calls `fmod`, `floor`, `ceil` and
# `trunc` (and their `f`-suffixed F32 forms). On a platform with a host, those
# calls resolve against the host's C runtime.

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    wide : F64
    wide = n.to_f64() + 4.5

    wide_divisor : F64
    wide_divisor = n.to_f64() - 1.0

    narrow : F32
    narrow = n.to_f32() + 4.5

    narrow_divisor : F32
    narrow_divisor = n.to_f32() - 1.0

    # The remainder keeps the sign of the dividend.
    Stdout.line!("rem: ${wide.rem_by(wide_divisor).to_str()} ${narrow.rem_by(narrow_divisor).to_str()} ${(0.0 - wide).rem_by(wide_divisor).to_str()}")
    Stdout.line!("div_trunc: ${wide.div_trunc_by(wide_divisor).to_str()} ${narrow.div_trunc_by(narrow_divisor).to_str()}")
    Stdout.line!("floor: ${Str.inspect(wide.floor_to_i64_try())} ${Str.inspect(narrow.floor_to_i64_try())}")
    Stdout.line!("ceiling: ${Str.inspect(wide.ceiling_to_i64_try())} ${Str.inspect(narrow.ceiling_to_i64_try())}")
}
