app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

str_err! : Str => (Str => [Exit(I32)])
str_err! = |stage| {
    |msg| {
        Stdout.line!("Failed to ${stage}: ${msg}")
        Exit(1)
    }
}

map_err! : Try(ok, a), (a => b) => Try(ok, b)
map_err! = |try, transform!| {
    match try {
        Err(a) => Err(transform!(a))
        Ok(ok) => Ok(ok)
    }
}

main! = || {
    result = map_err!(Err("error"), str_err!("parse"))
    match result {
        Ok(_) => {}
        Err(_) => Stdout.line!("Got error")
    }
}
