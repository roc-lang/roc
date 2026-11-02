app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A use of an alias is a literal edge to the alias's scheme: its arguments pass
# down the chain as dictionaries do, converted where the type becomes concrete.
Dollar := [D(I64)].{
    from_numeral : Numeral -> Try(Dollar, [InvalidNumeral(Str)])
    from_numeral = |_| Ok(D(100))

    plus : Dollar, Dollar -> Dollar
    plus = |D(a), D(b)| D(a + b)

    to_str : Dollar -> Str
    to_str = |D(a)| "$${a.to_str()}"
}

bump = |x| x + 1

at_u64 : U64 -> U64
at_u64 = bump

at_dollar : Dollar -> Dollar
at_dollar = bump

gen = bump
gen_again = gen

apply = |f, x| f(x)

wrap = |x| gen_again(x)

main! = || {
    Stdout.line!(at_u64(1).to_str())
    Stdout.line!(at_dollar(Dollar.D(1)).to_str())
    Stdout.line!(gen(2.U8).to_str())
    Stdout.line!(gen_again(3.I16).to_str())
    Stdout.line!(gen_again(Dollar.D(2)).to_str())
    Stdout.line!(apply(at_u64, 4).to_str())
    Stdout.line!(apply(gen_again, 5.U32).to_str())
    Stdout.line!(wrap(6.I64).to_str())
    Stdout.line!(wrap(Dollar.D(3)).to_str())
}
