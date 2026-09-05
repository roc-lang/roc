app [main!] {
    pf: platform "../pfroot/main.roc",
    benign: "./benign/main.roc",
}

import benign.Helper

main! = |_args| {
    _ = Helper.greet("world")
    Ok({})
}
