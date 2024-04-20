platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

SingleTagUnion : [OneTag]

mainForHost : SingleTagUnion
mainForHost = main
