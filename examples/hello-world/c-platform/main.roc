platform "hello-world-in-c"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str
mainForHost = main
