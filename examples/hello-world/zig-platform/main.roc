platform "hello-world-in-zig"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str
mainForHost = main
