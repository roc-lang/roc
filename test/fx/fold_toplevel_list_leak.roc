app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Tests List.fold with a module-level list.
# This test verifies there's no memory leak when a module-level list
# is passed to fold.
#
# Based on the minimal repro:
#   vals : List(U32)
#   vals = [1, 2, 3]
#   main! = |_args| { vals.fold(0, |acc, v| acc + v).to_str->Stdout.line!; Ok({}) }
#
# The difference from numeric_fold.roc is that the list is defined
# at module level rather than inside the main! function.

vals : List(Dec)
vals = [1, 2, 3]

main! = || {
    sum = vals.fold(0, |acc, v| acc + v)
    Stdout.line!(Dec.to_str(sum))
}
