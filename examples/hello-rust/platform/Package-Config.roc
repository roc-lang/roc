platform "hello-rust"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

mainForHost : Str
mainForHost = main
