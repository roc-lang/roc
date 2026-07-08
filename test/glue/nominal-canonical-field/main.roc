platform "glue-nominal-canonical-field"
    requires {
        main! : () => {}
    }
    exposes [Api]
    packages {}
    provides {
        "roc_main": main_for_host!,
        "roc_request": request_for_host,
    }
    hosted {}
    targets: {}

import Api

main_for_host! : () => {}
main_for_host! = main!

request_for_host : {} -> { id : Api.RequestId, count : U8 }
request_for_host = |_| { id: Api.RequestId.(42), count: 1 }
