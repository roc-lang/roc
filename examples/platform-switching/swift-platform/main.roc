platform "echo-in-swift"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str
mainForHost = main
