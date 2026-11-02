app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout

import Forwards

local_again = Forwards.again

local_at_u8 : U8 -> Str
local_at_u8 = Forwards.once_more

local_gen = Forwards.gen

apply = |f, x| f(x)

main! = || {
    Stdout.line!(Forwards.stored(1))
    Stdout.line!(Forwards.again(2.U16))
    Stdout.line!(Forwards.once_more(Forwards.F(3)))
    Stdout.line!(Forwards.at_u8(4))
    Stdout.line!(apply(Forwards.stored, 5))
    Stdout.line!(apply(Forwards.again, 6.I8))
    Stdout.line!(local_again(7.U8))
    Stdout.line!(local_at_u8(8))
    Stdout.line!(Forwards.at(9).to_str())
    Stdout.line!(Forwards.gen(10.U8).to_str())
    Stdout.line!(Forwards.gen_again(11.I32).to_str())
    Stdout.line!(local_gen(12.I16).to_str())
}
