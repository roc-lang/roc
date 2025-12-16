app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Wrapper := [
    WithRecord({ name : Str, value : U64 }),
    Simple(Str),
]

getName : Wrapper -> Str
getName = |w| match w {
    Wrapper.Simple(s) => s
    Wrapper.WithRecord(payload) => payload.name
}

main! = || {
    wrapper = Wrapper.WithRecord({ name: "hello", value: 42 })
    Stdout.line!(getName(wrapper))
}
