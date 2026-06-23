app [main!] { pf: platform "./static-lib-platform/main.roc" }

import pf.HostWrap

main! = |_seed| {
    token = HostWrap.wrap!(41)

    if HostWrap.unwrap(token) == 42 {
        "ok"
    } else {
        "bad"
    }
}
