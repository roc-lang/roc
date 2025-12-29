app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8738: Using ? operator on a non-Try type should give
# a clear "EXPECTED TRY TYPE" error, not "NON-EXHAUSTIVE MATCH"

ok_or : Try(ok, _err), ok -> ok
ok_or = |try, fallback|
    match try {
        _ => fallback
    }

do_something = || {
    # This should error: ok_or returns [Exit I32] which is not a Try type
    # (it has Exit tag instead of Ok/Err tag)
    _x = ok_or(Err(""), Exit(5))?
    Ok({})
}

main! = || {
    Stdout.line!("${Str.inspect(do_something())}")
}
