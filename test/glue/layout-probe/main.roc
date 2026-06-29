platform ""
    requires {
        main! : () => {}
    }
    exposes [Probe]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_probe_roundtrip": Probe.roundtrip!,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libunwind.a", "libc.a"], output: Exe },
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import Probe

main_for_host! : () => {}
main_for_host! = main!
