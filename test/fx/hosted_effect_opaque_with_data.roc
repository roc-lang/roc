app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

## Test that hosted effects work on opaque types with data.
## This is a regression test for a bug where hosted effects on opaque types
## with actual data (not just []) would crash with:
## "Roc crashed: Hosted functions cannot be called in the interpreter"
main! = || {
    # Create a Host with data using the factory function
    host = Host.new("World")

    # Call the hosted effect method on the opaque type with data
    greeting = host.get_greeting!()

    # Print the result
    Stdout.line!(greeting)
}
