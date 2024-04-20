platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

MyRcd : { a : U64, b : U128 }

mainForHost : MyRcd
mainForHost = main
