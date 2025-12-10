app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1, 2, 3]
    Stdout.line!("Method get")
    match list.get(0) {
        Ok(v) => Stdout.line!("Method get: ${v.to_str()}")
        Err(_e) => Stdout.line!("Error")
    }
    
    Stdout.line!("Qualified get")
    match List.get(list, 0) {
        Ok(v) => Stdout.line!("Qualified get: ${v.to_str()}")
        Err(_e) => Stdout.line!("Error")
    }
}
