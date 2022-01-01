platform "examples/multi-dep-thunk"
    requires {}{ main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : Str
mainForHost = main
