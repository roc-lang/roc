app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The derived encoder of `wrap` is generic over the error row each caller picks.
wrap = |_| Json.to_str_try({ n: 1.U64 })

main! = || {
    narrow : Try(Str, [Oops])
    narrow = wrap(0)
    wide : Try(Str, [Bad(Str), Worse])
    wide = wrap(0)
    Stdout.line!(narrow ?? "narrow failed")
    Stdout.line!(wide ?? "wide failed")
}
