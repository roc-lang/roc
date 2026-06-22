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
        inputs_dir: "../../wasm/platform/targets/",
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

import Probe

main_for_host! : () => {}
main_for_host! = main!
