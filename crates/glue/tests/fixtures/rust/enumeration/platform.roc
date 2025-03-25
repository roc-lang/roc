platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

MyEnum : [Foo, Bar, Baz]

main_for_host : MyEnum
main_for_host = main
