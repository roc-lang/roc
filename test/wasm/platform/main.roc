platform ""
    requires {} { main! : () => Str }
    exposes [Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Stdout.line!,
        "roc_stdout_unused_niche_feature": Stdout.unused_niche_feature!,
    }
    targets: {
        inputs_dir: "targets/",
        wasm32: { inputs: ["host.wasm", app] },
    }

import Stdout

main_for_host! : () => Str
main_for_host! = main!
