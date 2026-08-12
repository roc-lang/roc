platform "field-default-root-order"
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "../../platform/targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            import_memory,
            minimum_memory,
            maximum_memory,
            initial_stack_size,
            global_base,
        },
    }

import_memory = Zeroed
minimum_memory = 65536
maximum_memory = 65536
initial_stack_size = 14752
global_base = 6592

config : { retries : U8 ?? 3 }
config = {}

main_for_host! : () => Str
main_for_host! = main!
