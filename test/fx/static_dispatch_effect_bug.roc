app [main!] { pf: platform "./platform/main.roc" }

import pf.Builder
import pf.Stdout

main! = || {
    builder = Builder.new("test")

    # Test static dispatch on effect method
    builder.print_value!()
}
