platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

MyRcd : { a : U64, b : U128 }

main_for_host : MyRcd
main_for_host = main
