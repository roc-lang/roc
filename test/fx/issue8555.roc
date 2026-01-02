app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8555: dbg on U8 extracted from Ok result
# The bug was: method call syntax `list.first()` showed garbage decimal values.
# The fix: use call site return type instead of method's internal return type.
extract_ok_u8 : U8
extract_ok_u8 = {
    list : List(U8)
    list = [8u8, 7u8]

    # Using method call syntax - the bug is now fixed
    match list.first() {
        Err(_) => 0u8
        Ok(first) => first
    }
}

main! = || {
    dbg extract_ok_u8
    Stdout.line!("done")
}
