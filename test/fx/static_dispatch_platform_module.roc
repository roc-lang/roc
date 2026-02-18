app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Builder

main! = || {
    # Static dispatch with . syntax on platform module
    result = Builder.new("middle").add_prefix("start-").add_suffix("-end")

    Stdout.line!("Result: ${result.get_value()}")
}
