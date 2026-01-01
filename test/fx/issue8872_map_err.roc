app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for https://github.com/roc-lang/roc/issues/8872
# When using a polymorphic function that calls a lambda and wraps
# the result in a Try, the return value gets corrupted.
#
# The bug is in calling a lambda with a polymorphic return type `b`
# where `b` is used in the return type `Try(ok, b)`.

transform_err : Try({}, a), (a -> b) -> Try({}, b)
transform_err = |try, transform| match try {
    Err(a) => Err(transform(a))
    Ok(ok) => Ok(ok)
}

main! = || {
    err : Try({}, I32)
    err = Err(42)

    result = transform_err(err, |_e| "hello")
    match result {
        Ok(_) => Stdout.line!("Got Ok")
        Err(msg) => Stdout.line!("Got Err: ${msg}")
    }
}
