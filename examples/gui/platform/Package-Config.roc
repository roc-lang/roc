platform "gui"
    requires {} { render : _ }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

ButtonStyles : { bgColor : Rgba, borderColor : Rgba, borderWidth : F32, textColor : Rgba }

renderForHost : [ Button Elem ButtonStyles ((Box (F32, F32 -> F32)) as ClickHandler), Col (List Elem), Row (List Elem), Text Str ]
renderForHost = render
