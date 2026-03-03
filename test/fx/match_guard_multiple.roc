app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = match 5 {
        x if x > 100 => "huge"
        x if x < 0 => "negative"
        x if x > 0 => "positive"
        _ => "zero"
    }
    Stdout.line!(result)
}
