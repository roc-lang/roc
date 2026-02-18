app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Use a mutable variable to prevent compile-time evaluation
main! = || {
    # The var keyword creates a runtime variable that can't be constant-folded
    var $divisor = 0

    # This will trigger a division by zero error at runtime
    result = 42 / $divisor

    Stdout.line!("Result: ${U64.to_str(result)}")
}
