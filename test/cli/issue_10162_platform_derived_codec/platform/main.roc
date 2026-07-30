platform "issue-10162"
    requires { main! : () => {} }
    exposes [Model]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {}

import Model

main_for_host! : () => {}
main_for_host! = main!
