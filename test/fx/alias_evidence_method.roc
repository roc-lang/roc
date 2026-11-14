app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A dispatch through an alias, including its literal argument: a generic caller
# holds that argument in its own scope, outside the aliases' scopes.
show_value = |_self, value| value.to_str()
show_again = show_value

tick_value = |_self, value| value + 1
tick_again = tick_value

Foo := [F].{
    show = show_again

    fixed : Foo, U8 -> Str
    fixed = show_again

    tick = tick_again
}

generic_show = |obj, value| obj.show(value)
generic_tick = |value| Foo.F.tick(value)

main! = || {
    Stdout.line!(Foo.F.show(1.U64))
    Stdout.line!(Foo.F.show(2.U8))
    Stdout.line!(Foo.F.fixed(3))
    Stdout.line!(generic_show(Foo.F, 4.I16))
    Stdout.line!(Foo.F.tick(5.U64).to_str())
    Stdout.line!(generic_tick(6.U8).to_str())
    Stdout.line!(generic_tick(7.I32).to_str())
}
