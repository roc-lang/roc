platform "examples/add"
    requires {} { main : I64 -> I64 }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Unused {}

mainForHost : I64 -> I64
mainForHost = \a -> main a
