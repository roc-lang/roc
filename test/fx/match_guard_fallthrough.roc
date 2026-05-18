app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = match 3 {
        x if x > 100 => "big"
        _ => "small"
    }
    Stdout.line!(result)
}
