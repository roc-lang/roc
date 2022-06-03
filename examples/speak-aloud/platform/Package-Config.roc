platform "examples/speak-aloud"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

mainForHost : Str
mainForHost = main
