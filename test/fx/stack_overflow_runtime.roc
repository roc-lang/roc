app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# This function causes infinite recursion, leading to stack overflow at runtime.
# It cannot be tail-call optimized because there's work after the recursive call.
overflow : I64 -> I64
overflow = |n|
    # Prevent tail-call optimization by adding to the result after recursion
    overflow(n + 1) + 1

main! = || {
    # This will overflow the stack at runtime
    result = overflow(0)
    Stdout.line!("Result: ${I64.to_str(result)}")
}
