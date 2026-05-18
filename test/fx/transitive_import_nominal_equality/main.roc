app [main!] { pf: platform "../platform/main.roc" }

import pf.Stdout
import A

main! = || {
    Stdout.line!(Str.inspect(A.a == A.b))
}
