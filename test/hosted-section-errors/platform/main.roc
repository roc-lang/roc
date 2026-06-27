platform ""
    requires {
        main! : I64 => I64
    }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_double": Host.nonexistent!,
        "roc-host-bad": Host.double!,
        "roc_alloc": Host.double!,
        "roc__sneaky": Host.double!,
        "roc_main": Host.triple!,
    }
    targets: {
        inputs_dir: "targets/",
        arm64mac: { inputs: [app] },
        x64mac: { inputs: [app] },
        x64glibc: { inputs: [app] },
        arm64glibc: { inputs: [app] },
        x64musl: { inputs: [app] },
        arm64musl: { inputs: [app] },
        x64win: { inputs: [app] },
        arm64win: { inputs: [app] },
    }

import Host

main_for_host! : I64 => I64
main_for_host! = |n| main!(n)
