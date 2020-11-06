app Main provides [ main ] imports [ Effect ]



main : Effect.Effect {} as Fx
main =
    d = Effect.putChar 68
    e = Effect.putChar 69
    
    e |> Effect.after \{} -> e 
