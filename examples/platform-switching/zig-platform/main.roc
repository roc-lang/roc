platform "echo-in-zig"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : { bar: Str, foo: I64 -> I64 }
mainForHost = { bar: main, foo: \x -> x }
