app [main] { pf: platform "../../test-platform-simple-zig/main.roc" }

import Dep1

main : Str
main = Dep1.value1({})
