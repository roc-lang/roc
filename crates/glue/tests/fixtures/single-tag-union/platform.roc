platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

SingleTagUnion : [OneTag]

mainForHost : SingleTagUnion
mainForHost = main
