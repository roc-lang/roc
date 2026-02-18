app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    one : U8
    one = 1
    two : U8
    two = one.plus(one) # one + one
    Stdout.line!("two: ${two}")
}
