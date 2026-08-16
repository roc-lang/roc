platform ""
    requires {
        main! : () => {}
    }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_double": Host.double!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app] },
        arm64mac: { inputs: [app] },
        x64glibc: { inputs: [app] },
        arm64glibc: { inputs: [app] },
        x64musl: { inputs: [app] },
        x64v1musl: { inputs: [app] },
        arm64musl: { inputs: [app] },
        arm64v1musl: { inputs: [app] },
        x64win: { inputs: [app] },
        arm64win: { inputs: [app] },
    }

import Host

main_for_host! : () => {}
main_for_host! = main!
