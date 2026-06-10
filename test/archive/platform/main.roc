platform ""
    requires {
        main! : I64 => I64
    }
    exposes [Host]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        inputs: "targets/",
        x64mac: { inputs: ["libhost.a", app], output: Archive },
        arm64mac: { inputs: ["libhost.a", app], output: Archive },
        x64glibc: { inputs: ["libhost.a", app], output: Archive },
        arm64glibc: { inputs: ["libhost.a", app], output: Archive },
        x64musl: { inputs: ["libhost.a", app], output: Archive },
        arm64musl: { inputs: ["libhost.a", app], output: Archive },
        x64win: { inputs: ["host.lib", app], output: Archive },
        arm64win: { inputs: ["host.lib", app], output: Archive },
        wasm32: { inputs: [app], output: Archive },
    }

import Host

main_for_host! : I64 => I64
main_for_host! = |n| main!(n)
