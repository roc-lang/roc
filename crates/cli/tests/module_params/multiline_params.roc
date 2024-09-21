app [main] {
     pf: platform "../fixtures/multi-dep-str/platform/main.roc",
}

import MultilineParams {
    sendHttpReq: \_ -> crash "todo",
    getEnvVar: \_ -> crash "todo",
}

main =
    MultilineParams.hi