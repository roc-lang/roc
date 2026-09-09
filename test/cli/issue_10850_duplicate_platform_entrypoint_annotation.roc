# Repro for https://github.com/roc-lang/roc/issues/10850:
# a platform root that declares its provided entrypoint twice must be reported
# as an error, not published as a hosted procedure without a checked template.
platform ""
    requires {
        [Model : model] for main : { init : model }
    }
    exposes []
    packages {}
    provides { "roc_main": main }

main : { init : Model }

main : { init : Model }
