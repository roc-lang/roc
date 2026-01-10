app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8927
# Memory leak when using ? operator inside a loop that accumulates to a mutable variable
# The bug occurs when there's an Err in the list that causes early return

fold_try : List(Try(ok, err)) -> Try(List(ok), err)
fold_try = |tries| {
    var ok_list = []
    for a_try in tries {
        ok_list = ok_list.append(a_try?)
    }
    Ok(ok_list)
}

main! = || {
    # Include an Err to trigger the error path which exposes the memory leak
    tries : List(Try(I64, [NaN]))
    tries = [Ok(1), Ok(2), Err(NaN), Ok(4)]

    result = fold_try(tries)

    Stdout.line!(Str.inspect(result))
}
