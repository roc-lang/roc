app [main] {
    pf: platform "../fixtures/multi-dep-str/platform/main.roc"
}

import Api { appId: "one" } as App1
import Api { appId: "two" } as App2

main =
    """
    App1: $(App1.getUser 1)
    App2: $(App2.getUser 2)
    """
