app [main!] {
    pf: platform "../../fx-open/platform/main.roc",
    lib: "pkg/main.roc",
}

import lib.Bar
import pf.Stdout

main! = |_args| {
    _ = Bar.make({}).ping()
    Stdout.line!("continued after checking")
    Ok({})
}
