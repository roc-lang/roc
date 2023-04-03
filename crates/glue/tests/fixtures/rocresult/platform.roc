platform "test-platform"
    requires {} { main : Bool -> Result Str I32 }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Bool -> Result Str I32
mainForHost = \u -> main u
