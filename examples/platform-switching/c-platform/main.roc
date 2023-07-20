platform "echo-in-c"
    requires {} { main : Str }
    exposes [Base64]
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str
mainForHost = main
