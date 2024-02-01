platform "echo-in-rust"
    requires {} { main : Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str
mainForHost = main
