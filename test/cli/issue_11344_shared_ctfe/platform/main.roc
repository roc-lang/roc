platform ""
    requires {} { main : Str -> { known : Str, dynamic : Str } }
    exposes []
    packages {}
    provides { "roc_main": run }
    targets: {
        x64musl: { inputs: [app], output: Archive },
        wasm32: { inputs: [app], output: Archive },
    }

run : Str -> { known : Str, dynamic : Str }
run = |input| main(input)
