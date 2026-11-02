app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Stored aliases nothing annotates: checking leaves their requirements to the
# concrete callable, which each later use pins.
via = |value| value.to_str()

again = via

box = { f: again }

listed = [again, via]

pick = |flag| if flag again else via

main! = || {
    f = box.f
    Stdout.line!(f(1.U8))
    Stdout.line!(pick(True)(2.U16))
    Stdout.line!(Str.join_with(List.map(listed, |g| g(3.U8)), ","))
}
