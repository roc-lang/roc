app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Container(a) := { value : a }.{
    pure : a -> Container(a)
    pure = |v| { value: v }

    run : Container(a) -> a
    run = |c| c.value
}

main! = || {
    # Direct - works correctly
    Stdout.line!("Direct: ${Str.inspect(Bool.False)}")

    # Via Container.pure then run - BUG: shows "0" instead of "False"
    c1 = Container.pure(Bool.False)
    v1 = Container.run(c1)
    Stdout.line!("Via pure/run: ${Str.inspect(v1)}")
}
