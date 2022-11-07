app "hello"
    packages { pf: "platform/main.roc" }
    imports [
        pf.Api.{ Elem, Event, Bounds },
        pf.Model.{ Model },
        ]
    provides [program] { } to pf

init : Bounds -> Model
init = \_ -> { text: "Hello, World!" }

update : Model, Event -> Model
update = \model, _ -> model

render : Model -> List Elem
render = \model -> [Text { text: model.text }]

program = { init, update, render }
