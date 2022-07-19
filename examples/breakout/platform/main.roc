platform "gui"
    requires { Model } { program : _ }
    exposes [Game]
    packages {}
    imports [Game.{ Bounds, Elem, Event }]
    provides [programForHost]

# TODO allow changing the window title - maybe via a Task, since that shouldn't happen all the time
programForHost : {
    init : (Bounds -> Model) as Init,
    update : (Model, Event -> Model) as Update,
    render : (Model -> List Elem) as Render,
}
programForHost = program
