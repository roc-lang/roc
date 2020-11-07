platform folkertdev/foo
    provides [ mainForHost ]
    requires [ main ]
    imports []
    effects Effect
        { putChar : Int -> Effect {}, putLine : Str -> Effect {} }
