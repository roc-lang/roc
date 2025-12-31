app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that recursive closure calls don't leak stack memory.
# If stack memory is not properly restored after closure returns,
# deeply recursive functions will exhaust the interpreter's stack.

# A recursive function that creates stack allocations on each call.
# The large record forces stack allocation, and without proper cleanup,
# this will cause stack overflow even with modest recursion depth.
recursive_with_record : I64 -> I64
recursive_with_record = |n|
    if n <= 0 {
        0
    } else {
        # Create a record that must be allocated on the stack
        record = { a: n, b: n * 2, c: n * 3, d: n * 4 }
        record.a + recursive_with_record(n - 1)
    }

main! = || {
    # With proper stack restoration, this should work fine.
    # Without it, stack will overflow due to accumulated allocations.
    result = recursive_with_record(1000)
    Stdout.line!("result = ${I64.to_str(result)}")
}
