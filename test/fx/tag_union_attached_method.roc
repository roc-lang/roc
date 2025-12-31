app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    count : Iter({})
    count = It({})
    _ignore = count.identity()
    Stdout.line!("ok")
}

Iter(s) :: [It(s)].{
    identity : Iter(s) -> Iter(s)
    identity = |It(s_)| It(s_)
}
