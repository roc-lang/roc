app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import Api { app_id: "one", protocol: https }

https = \url -> "https://${url}"

main =
    "${Api.get_post}"
