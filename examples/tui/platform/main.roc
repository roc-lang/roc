platform "tui"
    requires { } { program : _ }
    exposes [Model]
    packages {}
    imports [
        Api.{ Bounds, Elem, Event },
        Model.{ Model },
        ]
    provides [programForHost]

# TODO allow changing the window title - maybe via a Task, since that shouldn't happen all the time
programForHost : {
    init : (Bounds -> Model) as Init,
    update : (Model, Event -> Model) as Update,
    render : (Model -> List Elem) as Render,
}
programForHost = program
