platform "examples/hello-world"
    requires {} { render : Elem }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Elem : [ Text Str, Button Elem ]

renderForHost : Elem
renderForHost = render
