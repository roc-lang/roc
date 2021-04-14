platform folkertdev/foo
    requires {}{foo:Str}
    exposes []
    packages {}
    imports [Cmd]
    provides [ mainForHost ]
    effects fx.Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : 
    {
        init : ({} -> { model: I64 as Model, cmd : (Cmd.Cmd [ Line Str ]) as Fx }) as Init,
        update : ([ Line Str ], I64 -> { model: I64, cmd : Cmd.Cmd [ Line Str ] } ) as Update
    }
mainForHost = main 
