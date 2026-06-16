platform ""
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {
        inputs_dir: "../platform/targets/",
        wasm32: { inputs: ["host.wasm", app], output: Shared },
    }

main_for_host! : () => Str
main_for_host! = main!
