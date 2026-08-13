platform "optional-field-host-boundary"
    requires {} { main! : List(Str) => { nested : { exit ?: I32 } } }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {}

main_for_host! = main!
