platform "gui"
    requires {} { program : Program State }
    exposes []
    packages {}
    imports []
    provides [ programForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

Bounds : { width : F32, height : F32 }

Elem : [ Rect { borderWidth : F32, color : Rgba, left : F32, top : F32, width : F32, height : F32 }, Text Str ]

State : { width : F32, height : F32 }

Program state : { render : state -> Elem }

# TODO allow changing the window title - maybe via a Task, since that shouldn't happen all the time
programForHost : { render : (State -> List Elem) as Render }
programForHost = program
