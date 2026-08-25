# Regression for a platform whose exposed module declares a hosted function the
# header's hosted section does not name: the hosted catalog is built from the
# section, so checking reports the missing entry instead of the compiler
# tripping over a catalog that outgrew its bindings.
app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! = |n| Host.double!(n)
