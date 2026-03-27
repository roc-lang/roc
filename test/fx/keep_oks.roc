app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test: Monomorphize panic "tag 'Err' missing from monotype"
# when a function matches on Try(ok, err) but is called with a callback
# that always returns Ok, so the Err tag is absent from the monotype.

keep_oks : List(a), (a -> Try(ok, _err)) -> List(ok)
keep_oks = |list, fun| {
    list.fold(
        [],
        |out_list, elem| {
            match fun(elem) {
                Ok(result) => out_list.append(result)
                Err(_) => out_list
            }
        }
    )
}

main! = || {
    always_ok_n = |_| Ok(1)

    # This would panic during monomorphization because the callback always
    # returns Ok, so the Err tag is missing from the monotype, but the
    # match in keep_oks expects both Ok and Err.
    equal_nums = (keep_oks([10], always_ok_n) == [1])

    if equal_nums {
        Stdout.line!("done")
    } else {
        Stdout.line!("FAIL: expected equal")
    }
}
