platform "examples/multi-dep-thunk"
    requires {}{ main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Unused {}

mainForHost : Str
mainForHost = main
