app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

apply! = |f, x| f(x)

main! = || {
    a = 10
    b = 20
    c = 30
    result =
        if Bool.True == Bool.True {
            apply!(|x| x + a + b, 5)
        } else {
            apply!(|x| x + b + c, 5)
        }
    Stdout.line!(result.to_str())
}
