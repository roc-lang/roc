app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Box2(a) :: { fun : U8 -> { val : a } }.{
    const : a -> Box2(a)
    const = |v| { fun: |_| { val: v } }

    run : Box2(a), U8 -> { val : a }
    run = |{ fun }, i| fun(i)
}

Maybe2(a) :: { fun : U8 -> { val ?: a } }.{
    const : a -> Maybe2(a)
    const = |v| { fun: |_| { val: v } }

    run : Maybe2(a), U8 -> { val ?: a }
    run = |{ fun }, i| fun(i)
}

main! = || {
    r = Box2.const(4.U64).run(0)
    o = Maybe2.const(5.U64).run(0)
    Stdout.line!(Str.inspect(r))
    Stdout.line!(Str.inspect(o))
}
