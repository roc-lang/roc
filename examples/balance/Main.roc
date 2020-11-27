app Main provides [ rocMain ] imports [ Effect ]

rocMain : Effect.Effect {} as Fx
rocMain =
    when List.len (Str.split "hello" "JJJJ there") is
        _  -> Effect.putLine "Yay"

