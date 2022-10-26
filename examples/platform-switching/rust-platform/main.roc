platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

MyRcd : { a : U64, b : U128, doStuff: Str -> Str }

mainForHost : MyRcd
mainForHost = main
