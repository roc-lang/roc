app "main" imports [ Effect ] provides [ rocMain ] to "./platform"


rocMain : Effect.Effect {} as Fx
rocMain =
    when List.len (Str.split "hello" "JJJJ there") is
        _  -> Effect.putLine "Yay"
