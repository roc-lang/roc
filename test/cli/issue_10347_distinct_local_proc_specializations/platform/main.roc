platform ""
    requires {
        main! : () => {}
    }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }

main_for_host! : () => {}
main_for_host! = main!
