app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests List.fold with numeric accumulators.

main! = || {
    sum = [1, 2, 3, 4, 5].fold(0, |acc, n| acc + n)
    Stdout.line!("Sum: ${I64.to_str(sum)}")
}
