app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Recursive function
factorial = |n|
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }

main! = || {
    result = factorial(5)
    Stdout.line!("5! = ${result.to_str()}")
}
