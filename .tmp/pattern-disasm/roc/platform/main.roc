platform ""
    requires {
        main : Str -> U64
    }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        arm64mac: { inputs: [app], output: Archive },
    }

main_for_host : Str -> U64
main_for_host = |input| main(input)
