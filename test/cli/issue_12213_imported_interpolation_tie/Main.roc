app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout
import Foo

main! = || {
    name : Str
    name = "Ada"
    Stdout.line!(Foo.describe({ name, age: 36.U64 }))
    Stdout.line!(Foo.describe({ name: 5.U64, age: 36.U64 }))
}
