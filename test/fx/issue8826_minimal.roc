app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
    Stdout.line!("Hello, World!")
    Ok({})
}
