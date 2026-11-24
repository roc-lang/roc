platform ""
    requires { main : () -> U64 }
    exposes [Host]
    packages {}
    provides { "roc_used": used, "roc_unused": unused! }
    hosted {
        "roc_native_only": Host.native_only!,
    }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app], exports: ["run"] },
    }

import Host

used : () -> U64
used = main

unused! : () => U64
unused! = || Host.native_only!()
