app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

# Regression test: F64 comparisons must use float comparison instructions,
# not integer bit-pattern comparison. Catches bugs where result_layout=Bool
# caused the codegen to use integer comparison for float operands.

main! = || {
    runtime = Host.get_greeting!(Host.new("float"))
    hasRuntime = Str.count_utf8_bytes(runtime) > 0

    a : F64
    a = if hasRuntime { 3.14 } else { -1.0 }

    b : F64
    b = if hasRuntime { 0.0 } else { 10.0 }

    if a > b {
        Stdout.line!("3.14 > 0.0: True")
    } else {
        Stdout.line!("3.14 > 0.0: False")
    }

    if b < a {
        Stdout.line!("0.0 < 3.14: True")
    } else {
        Stdout.line!("0.0 < 3.14: False")
    }

    if a >= 3.14 {
        Stdout.line!("3.14 >= 3.14: True")
    } else {
        Stdout.line!("3.14 >= 3.14: False")
    }
}
