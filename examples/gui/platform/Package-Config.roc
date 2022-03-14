platform "gui"
    requires {} { render : _ }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

ButtonStyles : { bgColor : Rgba, borderColor : Rgba, borderWidth : F32, textColor : Rgba }

OnClickHandler : Nat

renderForHost : [ Button Elem ButtonStyles OnClickHandler, Col (List Elem), Row (List Elem), Text Str ] as Elem
renderForHost = render
