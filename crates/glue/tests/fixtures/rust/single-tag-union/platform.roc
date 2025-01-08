platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

SingleTagUnion : [OneTag]

main_for_host : SingleTagUnion
main_for_host = main
