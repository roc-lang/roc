app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8521: App/Platform return type not fully unified
#
# This test is a minimal example to verify that the return type of a lambda
# is properly unified with the platform's expected type.

main! = || {
    Stdout.line!("Testing issue 8521")
}
