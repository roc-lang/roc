platform ""
    requires {
        [Model : model] for main : {
            modify : model -> model,
            encode : model -> Try(U64, []),
        }
    }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

main_for_host : Model -> Try(U64, [])
main_for_host = |m| (main.encode)(m)
