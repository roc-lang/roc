app [main] {
    pf: platform "../fixtures/multi-dep-str/platform/main.roc"
}

import Api { appId: "one" } as App1
import Api { appId: "two" } as App2

main : Str
main =
    """
    App1: $(Inspect.toStr (App1.postUrls 1))
    App2: $(Inspect.toStr (App2.postUrls 2))
    """
