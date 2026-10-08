app [main!] { pf: platform "./platform/main.roc" }

## A `main!` whose arity the platform rejects runs as the checked error it is,
## whether or not the program is specialized.
main! = |args| {
    _ = args
    {}
}
