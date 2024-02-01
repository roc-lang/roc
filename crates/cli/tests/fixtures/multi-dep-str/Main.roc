app "multi-dep-str"
    packages { pf: "platform/main.roc" }
    provides [main] to pf

import Dep1

main : Str
main = Dep1.str1
