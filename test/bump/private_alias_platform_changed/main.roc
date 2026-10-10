platform "bump"
    requires {}
    exposes [Public, identity]
    packages {}
    provides {}
    targets: {}

Public : { count: U64 }
Hidden : {}

identity : Public -> Public
identity = |value| value
