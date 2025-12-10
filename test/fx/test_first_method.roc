app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Try first with method syntax
    Stdout.line!("Calling method list.first()")
    match list.first() {
        Ok(v) => Stdout.line!("First: ${v.to_str()}")
        Err(_e) => Stdout.line!("Empty")
    }
}
