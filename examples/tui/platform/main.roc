platform "tui"
    requires { } { program : _ }
    exposes [Model]
    packages {}
    imports [
        Event.{ Bounds, Event },
        Elem.{ Elem },
        Model.{ Model },
        ]
    provides [programForHost]

programForHost : {
    init : (Bounds -> Model) as Init,
    update : (Model, Event -> Model) as Update,
    render : (Model -> List Elem) as Render,
}
programForHost = program
