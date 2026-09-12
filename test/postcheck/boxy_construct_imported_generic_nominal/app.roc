app [main!] { pf: platform "./platform/main.roc" }

import Container

main! = |_args| {
    value : Container(U8)
    value = { items: Str.to_utf8("x") }
    _ = value
    Ok({})
}
