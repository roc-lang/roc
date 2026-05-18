app [main!] { pf: platform "./platform/main.roc" }

# This tests that error messages display correctly without memory corruption
# when using the ? operator on a non-Try type
_a = A?

main! = || {}
