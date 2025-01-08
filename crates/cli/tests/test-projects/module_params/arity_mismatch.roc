app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import Api { app_id: "one", protocol: https }

https = \url -> "https://$(url)"

main =
    """
    # too many args
    $(Api.get_user(1, 2))
    $(Api.base_url(1))

    # too few args
    $(Api.get_post_comment(1))
    """
