platform ""
    requires {
        [Model : model] for main : {
            init : {} -> model,
            view : model -> V(model),
        },
    }
    exposes [V]
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        arm64mac: { inputs: ["libhost.a", app] },
    }

import V exposing [V]

main_for_host : {} -> Box(Model)
main_for_host = |{}| {
    init_fn = main.init
    view_fn = main.view
    m = init_fn({})
    _ = view_fn(m)
    Box.box(m)
}
