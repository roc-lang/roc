# A WASM-4 style layout: an imported, fixed 64 KiB memory whose addresses below
# global_base belong to the host, with a stack larger than global_base.
platform "issue-12554-reserved-low-memory"
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            import_memory: Zeroed,
            minimum_memory: 65536,
            maximum_memory: 65536,
            initial_stack_size: 14752,
            global_base: 6592, # 0x19c0
            exports: ["wasm_main", "wasm_result_len", "wasm_reset_alloc_counts", "wasm_alloc_count", "wasm_dealloc_count"],
        },
    }

main_for_host! : () => Str
main_for_host! = main!
