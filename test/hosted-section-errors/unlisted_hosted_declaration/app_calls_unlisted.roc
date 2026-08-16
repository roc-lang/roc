# Regression for calling the hosted function the header's hosted section leaves
# out: the section is what would give it a linker symbol, so there is nothing
# for the call to reach and the compile stops on checking's report of the
# missing entry.
app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! = |n| Host.unlisted!(n)
