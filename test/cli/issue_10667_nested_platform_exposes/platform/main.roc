platform "blub"
    requires {} { main! : {} => U64 }
    exposes [Container.Blub]
    packages {}
    provides { "roc_main": main_for_host! }
    targets: {}

import Container

main_for_host! = |_| main!({})
