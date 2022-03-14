platform "gui"
    requires {} { render : Elem }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

ButtonStyles : { bgColor : Rgba, borderColor : Rgba, borderWidth : F32, textColor : Rgba }

Elem : [ Button Elem ButtonStyles Nat, Col (List Elem), Row (List Elem), Text Str ]

renderForHost : Elem
renderForHost = render
