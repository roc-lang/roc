# Regression for a hosted declaration written in the platform root itself. A
# hosted section entry names its target through an import, so no entry can ever
# reach a declaration in the root, and checking reports it as missing from the
# section instead of the compiler stopping with nothing to say.
app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! = |n| Host.double!(n)
