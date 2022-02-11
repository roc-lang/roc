platform "hello-web"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

mainForHost : Str
mainForHost = main
