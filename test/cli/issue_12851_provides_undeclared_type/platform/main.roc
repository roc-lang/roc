platform ""
    requires {
        main : () -> U64
    }
    exposes []
    packages {}
    provides { "roc_run": run }

run : U64 -> Nope
run = |_| main()
