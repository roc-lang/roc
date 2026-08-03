app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout
import Foo

main! = || {
    Stdout.line!(Foo.parse("hello world"))
}
