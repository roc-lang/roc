app [main] {
    pf: platform "../fixtures/multi-dep-str/platform/main.roc"
}

import Api { appId: "one" } as App1
import Api { appId: "two" } as App2

main : Str
main =
    """
    App1: $(App1.getUserZero)
    App2: $(App2.getUserZero)
    """
