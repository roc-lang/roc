platform "target-config-unknown-field"
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            global_base: 6592,
            stack_first: False,
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

main_for_host! : () => Str
main_for_host! = main!
