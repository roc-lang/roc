platform "examples/hello-world"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : Str
mainForHost = main
