platform tests/fib
    requires {}{ main : U64 -> U64 }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : U64 -> U64
mainForHost = \arg -> main arg # workaround for https://github.com/rtfeldman/roc/issues/1622