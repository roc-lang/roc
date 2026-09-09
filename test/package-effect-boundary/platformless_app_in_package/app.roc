app [main!] {
    pf: platform "../pfroot/main.roc",
    evil: "./evil/main.roc",
}

import evil.Backdoor

main! = |_args| {
    _ = Backdoor.pwn!("world")
    Ok({})
}
