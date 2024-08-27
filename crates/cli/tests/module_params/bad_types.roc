app [main] {
    pf: platform "../fixtures/multi-dep-str/platform/main.roc",
}

import Api { appId: "one", protocol: https }

https = \url -> "https://$(url)"

main =
    """
    # too many args
    Api.getUser: $(Api.getUser 1 2)
    """
