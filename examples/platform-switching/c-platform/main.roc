platform "echo-in-c"
    requires {} { main : Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str
mainForHost = main
