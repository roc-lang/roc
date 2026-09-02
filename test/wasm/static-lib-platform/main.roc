platform ""
    requires {} { main! : U64 => Str }
    exposes [Runtime, HostWrap]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_host_wrap_token": HostWrap.wrap!,
        "roc_runtime_seed": Runtime.seed!,
    }
    targets: {
        inputs_dir: "../platform/targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            output: Shared,
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

import Runtime
import HostWrap

main_for_host! : () => Str
main_for_host! = || main!(Runtime.seed!())
