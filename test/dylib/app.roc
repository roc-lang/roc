app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! : I64 => I64
main! = |n| Host.double!(n) + 1
