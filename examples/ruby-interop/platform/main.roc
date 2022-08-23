platform "hello-world"
    requires {} { makeItRoc : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \str -> makeItRoc str
