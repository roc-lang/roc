app [main!] { pf: platform "./platform/platform.roc" }

import pf.Fallible

main! = |_args| {
    _ = Fallible.line!("hello")
    Ok({})
}
