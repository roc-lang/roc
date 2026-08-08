platform ""
    requires { make_handler : U64 -> Box(U64 -> U64) }
    exposes []
    packages {}
    provides { "roc_make": make_for_host }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

make_for_host : U64 -> Box(U64 -> U64)
make_for_host = make_handler
