platform ""
    requires { [Model : model] for program : { init : {} -> Box(Model) } }
    exposes []
    packages {}
    provides { "roc_init": init_for_host, "roc_release": release_for_host }
    targets: {
        x64musl: { inputs: [app], output: Archive },
    }

init_for_host = program.init
release_for_host : Box(Model) -> {}
release_for_host = |_model| {}
