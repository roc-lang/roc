platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Outer : { x : Inner, y : Str, z : List U8 }

Inner : { a : U16, b : F32 }

mainForHost : Outer
mainForHost = main
