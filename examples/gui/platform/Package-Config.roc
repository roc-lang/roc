platform "examples/hello-world"
    requires {} { render : Str }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]
    effects fx.Effect {}

renderForHost : Str
renderForHost = render
