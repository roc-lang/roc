platform "glue-package-nominal-api"
    requires {} { main : {} -> {} }
    exposes [Api]
    packages {
        pkg: "../pkg/main.roc",
    }
    provides { "roc_main": main_for_host, "roc_identity": identity_for_host }
    hosted {}
    targets: {}

import Api

main_for_host : {} -> {}
main_for_host = |{}| main({})

identity_for_host : Api.Thing -> Api.Thing
identity_for_host = |thing| thing
