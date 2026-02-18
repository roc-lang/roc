platform ""
    requires {
        main! : () => {}
    }
    exposes [Stdout, Stderr, Stdin, Builder, Host]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64win: ["host.lib", app],
            arm64win: ["host.lib", app],
        }
    }

import Stdout
import Stderr
import Stdin
import Builder
import Host

main_for_host! : () => {}
main_for_host! = main!
