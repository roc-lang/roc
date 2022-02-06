platform "examples/hello-world"
    requires {} { render : Str }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

renderForHost : Str
renderForHost = render
