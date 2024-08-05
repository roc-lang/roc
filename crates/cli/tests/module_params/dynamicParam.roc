app [main] {
    pf: platform "../fixtures/multi-dep-str/platform/main.roc"
}

userUrl = \appId, userId ->
    import Api { appId }
    Api.getUser userId

main =
    """
    App1: $(userUrl "one" 1)
    App2: $(userUrl "two" 2)
    """
