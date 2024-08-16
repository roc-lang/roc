platform "effects"
    requires {} { main : Effect.Effect {} }
    exposes []
    packages {}
    imports [Effect]
    provides [mainForHost]

mainForHost : Effect.Effect {}
mainForHost = main
