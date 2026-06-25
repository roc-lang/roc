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
        wasm32: { inputs: ["host.wasm", app] },
    }

import Stdout
import AnyValue

main_for_host! : () => Str
main_for_host! = main!
