app "breakout"
    packages { pf: "platform/main.roc" }
    imports [pf.Game.{ Bounds, Elem, Event }]
    provides [program] { Model } to pf

Model : { text : Str }

init : Bounds -> Model
init = \_ -> { text: "Hello, World!" }

update : Model, Event -> Model
update = \model, _ -> model

render : Model -> List Elem
render = \model -> [Text model.text]

program = { init, update, render }
