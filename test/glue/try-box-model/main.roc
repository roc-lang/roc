platform "glue-try-box-model"
    requires {
        [Model : model] for program : {
            init : {} -> model,
        }
    }
    exposes []
    packages {}
    provides {
        "roc_init": init_for_host,
    }
    targets: {}

init_for_host : {} -> Try(Box(Model), I32)
init_for_host = |{}| Ok(Box.box((program.init)({})))
