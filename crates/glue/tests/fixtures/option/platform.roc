platform "test-platform"
    requires {} { main : Bool -> [Some Str, None] }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Bool -> [Some Str, None]
mainForHost = \u -> main u
