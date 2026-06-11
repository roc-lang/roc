app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! : I64 => I64
main! = |n| Host.vanish!(Host.double!(n))
