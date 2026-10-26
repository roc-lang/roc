platform "stack-first-maximum-memory-too-large"
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            maximum_memory: 4294967296,
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

main_for_host! : () => Str
main_for_host! = main!
