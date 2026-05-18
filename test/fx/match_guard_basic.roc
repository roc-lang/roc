app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = match 5 {
        x if x > 0 => "positive"
        _ => "non-positive"
    }
    Stdout.line!(result)
}
