platform ""
    requires {
        main! : () => {}
    }
    exposes [Stdout, Stderr, Stdin, Builder, Host, NodeA, NodeB, Element]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        inputs: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

import Stdout
import Stderr
import Stdin
import Builder
import Host
import NodeA
import NodeB
import Element

main_for_host! : () => {}
main_for_host! = main!
