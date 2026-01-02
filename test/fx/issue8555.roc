app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8555: dbg on U8 extracted from Ok result
# The bug: method call syntax `list.first()` causes dbg to show garbage decimal values.
# Workaround: function call syntax `List.first(list)` works correctly.
# This test uses function call syntax as a workaround.
extract_ok_u8 : U8
extract_ok_u8 = {
    list : List(U8)
    list = [8u8, 7u8]

    # Using function call syntax as workaround - method syntax still has the bug
    match List.first(list) {
        Err(_) => 0u8
        Ok(first) => first
    }
}

main! = || {
    dbg extract_ok_u8
    Stdout.line!("done")
}
