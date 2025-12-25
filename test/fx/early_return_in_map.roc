app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test that early return (?) operator works correctly within closures passed to List.map.
#
# This tests for a bug where early return from a closure in List.map corrupted
# the value stack because the early_return continuation didn't clean up pending
# call_invoke_closure continuations properly.
#
# The bug manifested as crashes with messages like:
# - "call_invoke_closure: value_stack empty when popping function"
# - "TypeMismatch in call_invoke_closure continuation"

main! = || {
    # When map calls the closure with Err({}), the ? operator triggers early return.
    # This should work correctly and return [Ok(1), Err({})].
    result = [Ok(1), Err({})]
        .map(|x| Ok(x?))

    count = List.len(result)
    Stdout.line!("Count: ${count.to_str()}")
}
