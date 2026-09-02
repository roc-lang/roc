platform ""
    requires {} { main! : () => U64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app], exports: ["run"] },
    }

main_for_host! : () => U64
main_for_host! = main!
