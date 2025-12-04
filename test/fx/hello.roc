app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Sdtin

main! = || {
    Stdout.line!("What's your name?")
    name = Stdin.line!()
    Stdout.line!("Hello, ${name}")
}
