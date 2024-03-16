platform "echo-in-zig"
    requires {} { main : Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str
mainForHost = main
