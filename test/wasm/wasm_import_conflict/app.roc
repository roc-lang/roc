app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! = || {
    Host.put!(7)
    42
}
