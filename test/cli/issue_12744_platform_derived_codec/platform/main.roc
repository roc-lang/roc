platform ""
    requires {
        [Model : model, Msg : msg] for init : Str -> (Model, Msg)
    }
    exposes []
    packages {}
    provides { "roc_init": init_for_host }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
    }

init_for_host : Str -> (Box(Model), Box(Msg))
init_for_host = |flags| {
    (model, msg) = init(flags)
    (Box.box(model), Box.box(msg))
}
