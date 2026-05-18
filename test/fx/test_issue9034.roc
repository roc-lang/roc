app [main!] { pf: platform "./platform/main.roc" }

import pf.Builder
import pf.Stdout

# Test that platform-exposed opaque types can be used in type annotations
# This is a regression test for issue #9034
my_builder : Builder
my_builder = Builder.new("test")

main! = || {
    Stdout.line!(Builder.get_value(my_builder))
}
