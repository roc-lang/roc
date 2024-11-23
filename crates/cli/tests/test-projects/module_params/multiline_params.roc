app [main] {
    pf: platform "../test-platform-simple-zig/main.roc",
}

import MultilineParams {
    sendHttpReq: \_ -> crash "todo",
    getEnvVar: \_ -> crash "todo",
}

main =
    MultilineParams.hi
