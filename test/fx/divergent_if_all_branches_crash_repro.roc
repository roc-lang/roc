app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

choose : Bool -> Str
choose = |flag| {
    if flag {
        crash "true branch"
    } else {
        crash "false branch"
    }
}

main! = || {
    Stdout.line!(choose(False))
}
