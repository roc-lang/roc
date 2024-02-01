platform "echo-in-swift"
    requires {} { main : Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str
mainForHost = main
