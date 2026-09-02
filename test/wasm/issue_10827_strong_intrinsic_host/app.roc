app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    boxed = Box.box(|| 42)
    Box.unbox(boxed)()
}
