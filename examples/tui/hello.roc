app "hello"
    packages { pf: "platform/main.roc" }
    imports [
        pf.Api.{ Event, Bounds },
        pf.Model.{ Model },
        pf.Elem.{ Elem }
        ]
    provides [program] { } to pf

init : Bounds -> Model
init = \_ -> { text: "Luke!" }

update : Model, Event -> Model
update = \model, _ -> model

noStyle = {
    fg : None, 
    bg: None, 
    modifiers : [],
}

render : Model -> List Elem
render = \model -> [
    # Text { text: model.text }
    Paragraph ([[{text : "Ahoy there", style: noStyle},{text : model.text, style: noStyle}]]) {
        title : "Hello World",
        titleStyle : noStyle,
        titleAlignment : Left,
        borders : [ALL],
        borderStyle : noStyle,
        borderType : Double,
        style : noStyle,
    },
    ]

program = { init, update, render }
