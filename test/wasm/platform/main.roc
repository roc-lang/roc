platform ""
    requires {} { main! : () => Str }
    exposes [Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {
        inputs: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

import Stdout

main_for_host! : () => Str
main_for_host! = main!
