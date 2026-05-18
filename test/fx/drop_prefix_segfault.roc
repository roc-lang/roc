app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

demo_input = "L000000000000000000000000"

main! = || {
    Stdout.line!(demo_input.drop_prefix("L"))
}
