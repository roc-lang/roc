platform "hello-world-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

MyRcd : { a : U64, b : U128 }

mainForHost : MyRcd
mainForHost = main
