app [main!] { pf: platform "../../fx-open/platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10520
#
# Regression test: the erased-callable marking walk in Lambda Solved held
# span slices into the type store across recursive calls that append to the
# same store (`add` in the func case, expansions inside expandMonoRoot).
# When an append realloced the backing list, the outer slice dangled and
# `roc check` read poison (panic: index out of bounds, index 0xAAAAAAAA).
#
# The trigger needs function payloads inside a tag union, a recursive
# parameterized nominal wrapping that union, a concrete top-level
# instantiation of it (which the walk visits as an eval root even though
# main! never references it), and enough type volume that a store realloc
# lands mid-walk. The app targets the fx-open platform because its modules
# count toward that type volume, so swapping in a smaller platform can
# silently stop exercising the realloc.

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
