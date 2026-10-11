app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Float abs of negative zero, driven by a runtime value so the operation
# reaches the backends instead of being folded at compile time. Negative zero
# does not compare below zero, so its sign survives unless abs clears the sign
# bit itself; `to_bits` shows which zero came back.

main! = || {
    n = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }

    wide : F64
    wide = (n.to_f64() - 3.0) * -1.0

    narrow : F32
    narrow = (n.to_f32() - 3.0) * -1.0

    Stdout.line!("f64: ${wide.to_bits().to_str()} ${wide.abs().to_bits().to_str()} ${wide.abs().to_str()}")
    Stdout.line!("f32: ${narrow.to_bits().to_str()} ${narrow.abs().to_bits().to_str()} ${narrow.abs().to_str()}")
    Stdout.line!("negative: ${(wide - 1.5).abs().to_str()} ${(narrow - 1.5).abs().to_str()}")
}
