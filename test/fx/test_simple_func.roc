app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

add = |a, b| a + b

main! = || {
    result = add(1, 2)
    Stdout.line!("Result: ${result.to_str()}")
}
