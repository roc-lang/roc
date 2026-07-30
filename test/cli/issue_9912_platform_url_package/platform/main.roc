platform "unresolvable-package-repro"
    requires {} { main : {} -> {} }
    exposes [Api]
    packages {
        http: "./does_not_exist/main.roc",
    }
    provides { "roc_main": main_for_host }
    hosted {}
    targets: {}

import Api

main_for_host : {} -> {}
main_for_host = |{}| main({})
