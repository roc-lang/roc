# Repro for https://github.com/roc-lang/roc/issues/10520:
# function payloads inside a concrete recursive parameterized nominal should check.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

KeyEvent : { key : Str, code : Str, ctrl : Bool, shift : Bool, alt : Bool, meta : Bool, repeat : Bool, is_composing : Bool }

PointerEvent : { client_x : F64, client_y : F64, button : U8, buttons : U8, ctrl : Bool, shift : Bool, alt : Bool, meta : Bool }

Attr(msg) : [
    Boolean(Str, Bool),
    Key(Str),
    KeyHandler(Str, List(Str), Bool, Box(KeyEvent -> Box(msg))),
    MsgEvent(Str, Bool, Box(msg)),
    PointerHandler(Str, Bool, Box(PointerEvent -> Box(msg))),
    String(Str, Str),
    ValueEvent(Str, Box(Str -> Box(msg))),
    Visibility(Str, Str, Box(msg)),
]

Page(msg) := [
    Text(Str),
    Element(Str, List(Attr(msg)), List(Page(msg))),
].{
    text : Str -> Page(msg)
    text = |s| Text(s)
}

page_view : Page(Str)
page_view = Page.text("x")

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| Ok({})
