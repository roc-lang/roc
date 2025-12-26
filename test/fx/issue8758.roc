app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result : List((Str, Str))
    result = List.append([], ("hello", "world"))
    dbg(result)
    Stdout.line!("done")
}
