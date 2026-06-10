platform ""
    requires {} { main! : () => {} }
    exposes [Effect]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        inputs: "../../fx/platform/targets/",
        arm64mac: { inputs: ["libhost.a", app] },
        arm64musl: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["libhost.a", app] },
        arm64win: { inputs: ["host.lib", app] },
        x64win: { inputs: ["host.lib", app] },
    }

import Effect

main_for_host! : () => {}
main_for_host! = main!
