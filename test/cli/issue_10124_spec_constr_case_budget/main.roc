app [main!] {
    pf: platform "../../fx/platform/main.roc",
    repro: "./pkg/main.roc",
}

import pf.Stdin
import repro.FxEnv

main! = || {
    _ = FxEnv.var_str(Stdin.line!())
    {}
}
