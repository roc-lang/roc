app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1]
    Stdout.line!("List created")
    
    # Try last with method syntax (also uses list_get_unsafe)
    Stdout.line!("Calling method list.last()")
    match list.last() {
        Ok(v) => Stdout.line!("Last: ${v.to_str()}")
        Err(_e) => Stdout.line!("Empty")
    }
}
