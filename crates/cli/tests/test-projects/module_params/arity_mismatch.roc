app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import Api { appId: "one", protocol: https }

https = \url -> "https://$(url)"

main =
    """
    # too many args
    $(Api.getUser 1 2)
    $(Api.baseUrl 1)

    # too few args
    $(Api.getPostComment 1)
    """
