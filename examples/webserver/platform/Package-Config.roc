platform "examples/webserver"
    requires {}{ main : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
#    effects fx.Effect {}

mainForHost : Str -> Str
mainForHost = \arg -> main arg # workaround for https://github.com/rtfeldman/roc/issues/1622
