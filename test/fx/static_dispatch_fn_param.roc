app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test static dispatch on function parameters.
# Previously, `args.get(0)` crashed when `args` was a function parameter
# because the numeric literal wasn't getting the correct U64 type from
# the method signature.
process! = |args| {
    match args.get(0) {
        Ok(x) => Stdout.line!(x)
        Err(_) => Stdout.line!("error")
    }
}

main! = || {
    args = ["hello", "world"]
    process!(args)
}
