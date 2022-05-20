platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]

NonRecursive : [ Foo Str, Bar U128, Blah I32, Baz ]

mainForHost : NonRecursive
mainForHost = main
