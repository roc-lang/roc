platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

MyEnum : [Foo, Bar, Baz]

mainForHost : MyEnum
mainForHost = main
