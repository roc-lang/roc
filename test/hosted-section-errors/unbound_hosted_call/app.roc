app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

# `Host.vanish!` has no entry in the platform header's hosted section, so it has
# no linker symbol to call.
main! = || {
    _ = Host.vanish!(1)
    {}
}
