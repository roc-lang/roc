platform "glue-unresolved-by-value"
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

init_for_host : {} -> { value : Model, tag : I32 }
init_for_host = |{}| { value: (program.init)({}), tag: 0 }
