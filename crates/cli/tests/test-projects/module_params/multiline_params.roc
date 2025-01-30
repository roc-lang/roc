app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import MultilineParams {
    send_http_req: \_ -> crash("todo"),
    get_env_var: \_ -> crash("todo"),
}

main =
    MultilineParams.hi
