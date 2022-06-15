platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

MyEnum : [Foo, Bar, Baz]

mainForHost : MyEnum
mainForHost = main
