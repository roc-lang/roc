platform ""
    requires {} { main! : () => Str }
    exposes [Stdout, AnyValue]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Stdout.line!,
        "roc_stdout_unused_niche_feature": Stdout.unused_niche_feature!,
        "roc_any_value_clone": AnyValue.clone,
        "roc_any_value_get_tagged": AnyValue.get_tagged,
        "roc_any_value_store_tagged": AnyValue.store_tagged,
        "roc_any_value_take": AnyValue.take,
    }
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

import Stdout
import AnyValue

main_for_host! : () => Str
main_for_host! = main!
