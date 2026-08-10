platform "nested-types"
    requires {} { main! : {} => U16 }
    exposes [Container.Request, Container.Response]
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {}

import Container

main_for_host! = |_| main!({})
