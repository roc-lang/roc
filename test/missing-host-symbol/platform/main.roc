platform ""
    requires {
        main! : I64 => I64
    }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_double": Host.double!,
        "roc_host_vanish": Host.vanish!,
    }
    targets: {
        inputs_dir: "../../dylib/platform/targets/",
        arm64mac: { inputs: ["libhost.a", app], output: Shared },
        x64musl: { inputs: ["libhost.a", app], output: Shared },
        arm64musl: { inputs: ["libhost.a", app], output: Shared },
        x64win: { inputs: ["host.lib", app], output: Shared },
        arm64win: { inputs: ["host.lib", app], output: Shared },
    }

import Host

main_for_host! : I64 => I64
main_for_host! = |n| main!(n)
