platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

Outer : { x : Inner, y : Str, z : List U8 }

Inner : { a : U16, b : F32 }

main_for_host : Outer
main_for_host = main
