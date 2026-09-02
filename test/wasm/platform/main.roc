platform ""
    requires {} { main! : () => Str }
    exposes [Stdout, FallibleHost]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_fallible_str_ok": FallibleHost.str_ok!,
        "roc_json_input": FallibleHost.json_input!,
        "roc_stdout_line": Stdout.line!,
        "roc_stdout_unused_niche_feature": Stdout.unused_niche_feature!,
    }
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

import Stdout
import FallibleHost

main_for_host! : () => Str
main_for_host! = main!
