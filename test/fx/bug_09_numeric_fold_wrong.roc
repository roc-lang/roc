app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Bug 9: Numeric fold produces incorrect values
# Expected: Sum: 15
# Actual: Sum: -3446744073709551616 (or similar garbage)
# Note: String fold works correctly

main! = || {
    sum = [1, 2, 3, 4, 5].fold(0, |acc, n| acc + n)
    Stdout.line!("Sum: ${I64.to_str(sum)}")
}
