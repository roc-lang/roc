app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr

demo_input = "L000000000000000000000000"

main! = || {
    _suffix = demo_input.drop_prefix("L")
}
