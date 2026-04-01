platform ""
    requires {} { main! : () => Str }
    exposes [Stdout]
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: {
            wasm32: ["host.wasm", app],
        }
    }

import Stdout

main_for_host! : () => Str
main_for_host! = main!
