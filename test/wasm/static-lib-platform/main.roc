platform ""
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        inputs: "../platform/targets/",
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

main_for_host! : () => Str
main_for_host! = main!
