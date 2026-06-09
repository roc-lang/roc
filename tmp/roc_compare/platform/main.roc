platform ""
    requires {} { main! : () => List(Plant) }
    exposes [Plant]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: {
            arm64mac: ["libhost.a", app],
            x64mac: ["libhost.a", app],
            arm64musl: ["libhost.a", app],
            x64musl: ["libhost.a", app],
        }
    }

import Plant

main_for_host! : () => List(Plant)
main_for_host! = || main!()
