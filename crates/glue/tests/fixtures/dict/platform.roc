platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Dict Str Str
mainForHost = main
