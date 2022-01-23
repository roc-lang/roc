app "hello"
    packages { pf: "platform" }
    imports []
    provides [ render ] to pf

render :
    { width : F32, height : F32 } ->
    [
        Rectangle { top : F32, left : F32, bottom : F32, right : F32 },
        Circle { top : F32, left : F32, radius : F32 },
        # Text { top : F32, left : F32, text : Str },
    ]
render = \window ->
    Rectangle { top: 10, left: 10, bottom: 100, right: 100 }
