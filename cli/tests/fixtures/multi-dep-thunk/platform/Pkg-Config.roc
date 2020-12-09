platform examples/multi-dep-thunk
    requires { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect {}

mainForHost : Str
mainForHost = main
