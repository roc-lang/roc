platform "examples/hello-world"
    requires {} { main : { content : Str, other : Str } }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : { content : Str, other : Str }
mainForHost = main
