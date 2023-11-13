platform "test-platform"
    requires {} { main : I64 -> ({} -> I64) }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : I64 -> ({} -> I64)
mainForHost = \x -> main x
