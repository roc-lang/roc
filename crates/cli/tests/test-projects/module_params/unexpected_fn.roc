app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import Api { appId: "one", protocol: https }

https = \url -> "https://$(url)"

main =
    """
    $(Api.getPost)
    """
