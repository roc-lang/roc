app [main!] { pf: platform "../../fx/platform/main.roc" }

import Bar

foo : Str -> Try(U64, Bar.SomeErrors)
foo = |_| {
    Ok(0)
}

main! = || {
    _ = foo("hello")
    {}
}
