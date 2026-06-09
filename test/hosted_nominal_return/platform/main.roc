platform ""
    requires {} { main! : () => {} }
    exposes [Effect]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "../../fx/platform/targets/",
        exe: {
            arm64mac: { files: ["libhost.a", app] },
            arm64musl: { files: ["libhost.a", app] },
            x64musl: { files: ["libhost.a", app] },
            arm64win: { files: ["host.lib", app] },
            x64win: { files: ["host.lib", app] },
        }
    }

import Effect

main_for_host! : () => {}
main_for_host! = main!
