platform "examples/hello-world"
    requires {} { render : {} -> { content : Str} }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]
    effects fx.Effect {}

renderForHost : {} -> { content : Str }
renderForHost = render
