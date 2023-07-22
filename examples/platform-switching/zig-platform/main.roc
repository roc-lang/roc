platform "echo-in-zig"
    requires {} { main : {} -> (Str -> Str) }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : {} -> (Str -> Str)
mainForHost = main
