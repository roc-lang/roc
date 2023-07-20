interface Dep
    exposes [strIdentity]
    imports []

strIdentity : Str -> Str
strIdentity = \input ->
    expect Bool.true

    input
