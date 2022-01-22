platform "examples/tea"
    requires {} {
        render :
            { width : F32, height : F32 } ->
            [
                Rectangle { top : F32, left : F32, bottom : F32, right : F32 },
                Circle { top : F32, left : F32, radius : F32 },
                # Text { top : F32, left : F32, text : Str },
            ]
    }
    exposes []
    packages {}
    imports []
    provides [ renderForHost ]
    effects fx.Effect {}

renderForHost :
    { width : F32, height : F32 } ->
    [
        Rectangle { top : F32, left : F32, bottom : F32, right : F32 },
        Circle { top : F32, left : F32, radius : F32 },
                # Text { top : F32, left : F32, text : Str },
    ]
renderForHost = render
