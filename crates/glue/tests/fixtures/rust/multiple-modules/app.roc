app [main] { pf: platform "platform.roc" }

import pf.Dep1
import pf.Dep2

main = { s1: Dep1.string("hello"), s2: Dep2.string("world") }
