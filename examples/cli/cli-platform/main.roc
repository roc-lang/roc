platform "cli"
    requires {} { main : Effect Str }
    exposes [Effect]
    packages {}
    imports [Effect.{ Effect }]
    provides [mainForHost]

mainForHost : Effect Str as Fx
mainForHost = main
