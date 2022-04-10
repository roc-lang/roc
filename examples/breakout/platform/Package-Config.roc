platform "gui"
    requires { Model } { program : _ }
    exposes []
    packages {}
    imports []
    provides [ programForHost ]

Rgba : { r : F32, g : F32, b : F32, a : F32 }

Elem : [ Rect { color : Rgba, left : F32, top : F32, width : F32, height : F32 }, Text Str ]

Event : [ Resize { width : F32, height : F32 }, KeyDown U32, KeyUp U32 ]

# TODO allow changing the window title - maybe via a Task, since that shouldn't happen all the time
programForHost : {
    init : ({} -> Model) as Init,
    update : (Model, Event -> Model) as Update,
    render : (Model -> List Elem) as Render
}
programForHost = program
