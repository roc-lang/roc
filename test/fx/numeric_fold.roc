app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests List.fold with numeric accumulators.
# TODO: Using Dec.to_str because type inference doesn't backpropagate
# from I64.to_str to constrain the fold result type. Once that's fixed,
# this test should work with I64.to_str without explicit type annotations.

main! = || {
    nums = [1, 2, 3, 4, 5]
    sum = nums.fold(0, |acc, n| acc + n)
    Stdout.line!("Sum: ${Dec.to_str(sum)}")
}
