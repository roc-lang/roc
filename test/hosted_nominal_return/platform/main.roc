platform ""
    requires {} { main! : () => {} }
    exposes [Effect]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "../../fx/platform/targets/",
        exe: {
            arm64mac: ["libhost.a", app],
            arm64musl: ["libhost.a", app],
            x64musl: ["libhost.a", app],
            arm64win: ["host.lib", app],
            x64win: ["host.lib", app],
        }
    }

import Effect

main_for_host! : () => {}
main_for_host! = main!
