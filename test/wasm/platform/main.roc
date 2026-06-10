platform ""
    requires {} { main! : () => Str }
    exposes [Stdout]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        inputs: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

import Stdout

main_for_host! : () => Str
main_for_host! = main!
