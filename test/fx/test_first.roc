app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1, 2, 3]
    Stdout.line!("Qualified first")
    match List.first(list) {
        Ok(v) => Stdout.line!("Qualified first: ${v.to_str()}")
        Err(_e) => Stdout.line!("Error")
    }
}
