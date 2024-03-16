platform "fibonacci"
    requires {} { main : I64 -> I64 }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : I64 -> I64
mainForHost = \a -> main a
