platform "effects"
    requires {} { main : Effect.Effect {} }
    exposes []
    packages {}
    imports [pf.Effect]
    provides [mainForHost]

mainForHost : Effect.Effect {} as Fx
mainForHost = main
