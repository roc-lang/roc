platform ""
    requires { main! : U64 => U64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            output: Shared,
            exports: ["roc_main"],
        },
    }

main_for_host! : U64 => U64
main_for_host! = |seed| main!(seed)
