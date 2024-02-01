app "hello-gui"
    packages { pf: "platform/main.roc" }
    provides [program] { Model } to pf

import pf.Game exposing [Bounds, Elem, Event]

Model : { text : Str }

init : Bounds -> Model
init = \_ -> { text: "Hello, World!" }

update : Model, Event -> Model
update = \model, _ -> model

render : Model -> List Elem
render = \model -> [Text { text: model.text, top: 0, left: 0, size: 40, color: { r: 1, g: 1, b: 1, a: 1 } }]

program = { init, update, render }
