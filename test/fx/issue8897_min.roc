app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Issue 8897: This would panic with "getExpr: unexpected tag=type_header"
# when running two expects with a polymorphic function.
# The fix ensures interpreter.env is reset to the test module's env
# before each test evaluation.

# Using concrete types to test the panic fix, because polymorphic
# function evaluation has a separate bug that causes incorrect results.
nth : List(Str), U64 -> Try(Str, [Nope])
nth = |l, i| {
  match List.get(l, i) {
    Ok(e) => Ok(e)
    Err(OutOfBounds) => Err(Nope)
  }
}

expect nth(["a", "b", "c", "d", "e"], 2) == Ok("c")
expect nth(["a"], 2) == Err(Nope)

main! = || {
    Stdout.line!("done")
}
