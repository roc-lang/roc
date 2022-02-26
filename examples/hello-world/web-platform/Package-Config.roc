platform "hello-world-in-web-assembly"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

mainForHost : Str
mainForHost = main
