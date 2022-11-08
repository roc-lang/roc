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


style1 = {bg: Blue, fg:Magenta, modifiers : [BOLD, ITALIC]}
style2 = {bg: Black, fg:LightRed, modifiers : [SLOWBLINK, UNDERLINED]}
style3 = {bg: Cyan, fg:LightMagenta, modifiers : []}
style4 = {bg: None, fg:Yellow, modifiers : []}

render : Model -> List Elem
render = \model -> [
    Paragraph ([[{text : "Ahoy there ", style: style1},{text : model.text, style: style2}]]) {
        title : "My Roc Box",
        titleStyle : style1,
        titleAlignment : Center,
        borders : [TOP,LEFT],
        borderStyle : style3,
        borderType : Double,
        style : style4,
    },
    ]

program = { init, update, render }
