app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# This function causes infinite recursion, leading to stack overflow at runtime.
# It cannot be tail-call optimized because there's work after the recursive call.
overflow : I64 -> I64
overflow = |n|
    # Prevent tail-call optimization by adding to the result after recursion
    overflow(n + 1) + 1

main! = || {
    # The var keyword creates a runtime variable that can't be constant-folded.
    var $start = 0.I64

    # This will overflow the stack at runtime
    result = overflow($start)
    Stdout.line!("Result: ${I64.to_str(result)}")
}
