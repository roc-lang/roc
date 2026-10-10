platform "bump"
    requires {}
    exposes [Public, identity]
    packages {}
    provides {}
    targets: {}

Public : {}
Hidden : {}

identity : Public -> Public
identity = |value| value
