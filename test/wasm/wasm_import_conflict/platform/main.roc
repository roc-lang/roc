platform ""
    requires {} { main! : () => U64 }
    exposes [Host]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_conflicting_put": Host.put!,
    }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app], exports: ["run"] },
    }

import Host

main_for_host! : () => U64
main_for_host! = main!
