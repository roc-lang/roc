platform ""
    requires {
        main! : I64 => I64
    }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_double": Host.double!,
        "roc_host_unused_niche_feature": Host.unused_niche_feature!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app], output: Shared },
        arm64mac: { inputs: ["libhost.a", app], output: Shared },
        x64glibc: { inputs: ["libhost.a", app], output: Shared },
        arm64glibc: { inputs: ["libhost.a", app], output: Shared },
        x64win: { inputs: ["host.lib", app], output: Shared },
        arm64win: { inputs: ["host.lib", app], output: Shared },
    }

import Host

main_for_host! : I64 => I64
main_for_host! = |n| main!(n)
