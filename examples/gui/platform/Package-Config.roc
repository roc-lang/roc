platform "examples/hello-world"
    requires {} { render : Elem }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

renderForHost : [ Button Elem { bgColor : Rgba, borderColor : Rgba,  borderWidth : F32, onClick : (Str -> Str), textColor : Rgba }, Col (List Elem), Row (List Elem), Text Str ] as Elem
renderForHost = render
