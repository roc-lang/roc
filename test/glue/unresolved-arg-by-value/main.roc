platform "glue-unresolved-arg-by-value"
    requires {
        [Model : model] for program : {
            render : model -> Str,
        }
    }
    exposes []
    packages {}
    provides {
        "roc_render": render_for_host,
    }
    targets: {}

render_for_host : Model -> Str
render_for_host = |model| (program.render)(model)
