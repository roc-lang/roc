app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    a = 100
    b = 200
    c = 300
    d = 400
    add_all = |n| a + b + c + d + n
    Stdout.line!(add_all(5).to_str())
}
