app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Picky := [P].{
    from_numeral : Numeral -> Try(Picky, [InvalidNumeral(Str)])
    from_numeral = |_| Err(InvalidNumeral("picky rejects every literal"))

    plus : Picky, Picky -> Picky
    plus = |a, _| a

    to_str : Picky -> Str
    to_str = |_| "picky"
}

bump = |x| x + 1

gen = bump

wrap = |x| gen(x)

main! = || {
    Stdout.line!(wrap(Picky.P).to_str())
}
