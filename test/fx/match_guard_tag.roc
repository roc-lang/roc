app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    value : [Some(I64), None]
    value = Some(10)
    result = match value {
        Some(n) if n > 5 => "big some"
        Some(_) => "small some"
        None => "none"
    }
    Stdout.line!(result)
}
