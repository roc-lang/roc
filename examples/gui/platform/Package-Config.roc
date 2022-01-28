platform "examples/hello-world"
    requires {} { render : { content : Str, title : Str } }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]
    effects fx.Effect {}

renderForHost : { content : Str, title : Str }
renderForHost = render
