app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

## Test wildcard match on open tag union errors
## Bug: When error propagates through open tag unions,
## Err(_) wildcard match produces zero instead of the expected value

# Simulates a hosted effect that returns Try with open error type
read_something! : {} => Try(Str, [NotFound, ..])
read_something! = |{}| Err(NotFound)

# Simulates an init function with its own error types plus open extension
# This is like: init! : Host => Try(Model, [Exit(I64), ..])
# When read_something! fails with NotFound, it propagates through the ".."
do_init! : {} => Try(Str, [Exit(I64), ..])
do_init! = |{}| {
    # This should propagate NotFound through the ".." in our error type
    result = read_something!({})?
    Ok(result)
}

# Platform converts open error to fixed type - this is where the bug appears
# When error is Exit(code), extract code; otherwise use wildcard for any other error
wrap_init! : {} => Try(Str, I64)
wrap_init! = |{}| {
    match do_init!({}) {
        Ok(s) => Ok(s)
        Err(Exit(code)) => Err(code)
        Err(_) => Err(42)  # BUG: This should return 42 but may return 0
    }
}

main! = || {
    # This will cause NotFound to propagate through ".." and hit the Err(_) branch
    result = wrap_init!({})

    code = match result {
        Ok(_) => 999
        Err(c) => c
    }

    # Should print 42 (the wildcard match result), but bug causes 0
    Stdout.line!("Error code: ${Str.inspect(code)}")

    if code == 42 {
        Stdout.line!("PASS: Wildcard match worked correctly")
    } else if code == 0 {
        Stdout.line!("FAIL: Bug reproduced - got 0 instead of 42")
    } else {
        Stdout.line!("UNEXPECTED: Got ${Str.inspect(code)}")
    }
}
