platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : U8
main_for_host = main
