platform "gui"
    requires {} { program : Program State }
    exposes []
    packages {}
    imports []
    provides [ programForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

ButtonStyles : { bgColor : Rgba, borderColor : Rgba, borderWidth : F32, textColor : Rgba }

Elem : [ Button Elem ButtonStyles, Col (List Elem), Row (List Elem), Text Str ]

State : { width : Str, height : Str } # TODO change from U32 to F32 once Num.toStr supports floats

Program state : { render : state -> Elem }

# TODO allow changing the title - maybe via Action.setTitle
programForHost : { render : (State -> Elem) as Render }
programForHost = program
