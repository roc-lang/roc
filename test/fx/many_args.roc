app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: function with 7+ arguments exercises stack spilling on x86_64
# (which has 6 integer argument registers). On aarch64, spilling starts
# at 9 args. Using 8 args covers both architectures.

add_all : I64, I64, I64, I64, I64, I64, I64, I64 -> I64
add_all = |a, b, c, d, e, f, g, h| a + b + c + d + e + f + g + h

main! = || {
    result = add_all(1, 2, 3, 4, 5, 6, 7, 8)
    Stdout.line!(result.to_str())
}
