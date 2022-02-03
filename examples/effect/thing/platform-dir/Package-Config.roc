platform "folkertdev/foo"
    requires {} { main : Effect {} }
    exposes []
    packages {}
    imports [ pf.Effect ]
    provides [ mainForHost ]
    effects fx.Unused {}

mainForHost : Effect.Effect {} as Fx
mainForHost = main
