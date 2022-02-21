platform "examples/hello-world"
    requires {} { render : [ Text Str, Button Str ] }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

renderForHost : [ Text Str, Button Str ]
renderForHost = render
