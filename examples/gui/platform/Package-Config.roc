platform "examples/hello-world"
    requires {} { render : Elem }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Dim : { left : F32, top : F32, width : F32, height : F32 }

Elem : [ Button Elem Dim, Col (List Elem), Row (List Elem), Text Str ]

renderForHost : Elem
renderForHost = render
