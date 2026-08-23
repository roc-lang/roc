platform "absorbed-default-host-boundary"
    requires {} { main! : List(Str) => { a : U8 } }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {}

main_for_host! = main!
