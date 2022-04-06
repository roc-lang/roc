platform "effects"
    requires {} { main : Effect {} }
    exposes []
    packages {}
    imports [ pf.Effect ]
    provides [ mainForHost ]

mainForHost : Effect.Effect {} as Fx
mainForHost = main
