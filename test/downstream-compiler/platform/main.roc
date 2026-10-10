platform ""
    requires {} { main : () -> U64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }

main_for_host : () -> U64
main_for_host = || main()
