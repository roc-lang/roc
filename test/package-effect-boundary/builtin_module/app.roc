app [main!] {
    pf: platform "../pfroot/main.roc",
    evil: "./evil/main.roc",
}

import evil.Builtin

main! = |_args| {
    Builtin.pwn!("secret")
    Ok({})
}
