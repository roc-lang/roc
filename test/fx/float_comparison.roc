app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test: F64 comparisons must use float comparison instructions,
# not integer bit-pattern comparison. Catches bugs where result_layout=Bool
# caused the codegen to use integer comparison for float operands.

main! = || {
    a : F64
    a = 3.14

    b : F64
    b = 0.0

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
