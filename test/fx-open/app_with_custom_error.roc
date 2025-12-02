app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# This example demonstrates the open union error type.
# The platform expects Try({}, [Exit(I32), ..others]) which means
# apps can return custom error tags not defined in the platform.
# The platform's wildcard match (_ => 1) handles unknown errors.
main! = |_args| {
    Stdout.line!("Returning custom error tag")
    # This custom error tag is NOT defined in the platform's type signature,
    # but it works because of the open union extension [Exit(I32), ..others]
    Err(CustomError)
}
