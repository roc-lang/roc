platform folkertdev/foo
    provides [ mainForHost ]
    requires [ main ]
    imports []
    effects
        { putChar : Int -> Effect {}, putLine : Str -> Effect {} }
