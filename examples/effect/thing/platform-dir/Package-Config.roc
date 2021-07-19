platform folkertdev/foo
    requires {model=>Model, msg=>Msg} {main : Effect {}}
    exposes []
    packages {}
    imports [fx.Effect]
    provides [ mainForHost ]
    effects fx.Effect
        {
            putLine : Str -> Effect {},
            getLine : Effect Str
        }



mainForHost : Effect.Effect {} as Fx
mainForHost = main
