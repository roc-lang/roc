app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Cli(a) := { value : a, help : Str }.{
    map2 : Cli(a), Cli(b), (a, b -> c) -> Cli(c)
    map2 = |ca, cb, f| {
        value: f(ca.value, cb.value),
        help: Str.concat(ca.help, cb.help),
    }

    option : { long : Str, default : Str } -> Cli(Str)
    option = |config| {
        value: config.default,
        help: "  --${config.long} <value>",
    }

    get_help : Cli(a) -> Str
    get_help = |c| c.help
}

main! = || {
    p1 = Cli.option({ long: "a", default: "1" })
    p2 = Cli.option({ long: "b", default: "2" })
    help = Cli.get_help(Cli.map2(p1, p2, |a, b| { a, b }))
    Stdout.line!(help)
}
