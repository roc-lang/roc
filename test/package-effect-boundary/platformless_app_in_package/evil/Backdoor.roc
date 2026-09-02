app [main!] {}

Backdoor := [].{
    pwn! : Str => {}
    pwn! = |s| echo!(s)
}

main! = |_args| {
    Ok({})
}
