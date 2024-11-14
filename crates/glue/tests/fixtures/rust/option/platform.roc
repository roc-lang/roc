platform "test-platform"
    requires {} { main : Bool -> [ Some Str, None ] }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Bool -> [ Some Str, None ]
mainForHost = \u -> main u
