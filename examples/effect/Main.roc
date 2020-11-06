app Main provides [ main ] imports [ Effect ]



main : Effect.Effect {} as Fx
main =
    e = Effect.putChar 68
    
    e |> Effect.after \{} -> e 
