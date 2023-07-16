platform "test-platform"
    requires {} { main : _ }
    exposes [Effect]
    packages {}
    imports [Effect.{ Effect }]
    provides [mainForHost]

mainForHost : Effect {}
mainForHost = main
