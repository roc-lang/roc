app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

count = |n, acc| if n == 0 { acc.to_str() } else { again(n - 1, acc + 1) }
again = count

at : U8, U16 -> Str
at = again

main! = || {
    Stdout.line!(again(3.U8, 0.U64))
    Stdout.line!(at(2, 5))
}
