platform folkertdev/foo
    requires {}
    exposes []
    packages {}
    imports [Cmd]
    provides [ mainForHost ]
    effects Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }


init = \{} -> 
    cmd = 
        Cmd.after (Cmd.putLine "Type a thing, and I'll say it back") \{} -> 
            Cmd.getLine (\l -> Line l) 

    { model: 42, cmd }

update = \msg, model -> 
    when msg is
        Line line -> 
            cmd = 
                Cmd.after (Cmd.putLine "You said:") \{} ->                             
                Cmd.after (Cmd.putLine line) \{} ->                             
                Cmd.after (Cmd.putLine "Type another thing, and I'll say it back") \{} ->                             
                Cmd.getLine (\l -> Line l) 
                    
            { model: model + 1, cmd }


mainForHost : 
    {
        init : ({} -> { model: I64, cmd : (Cmd.Cmd [ Line Str ]) as Fx }) as Init,
        update : ([ Line Str ], I64 -> { model: I64, cmd : Cmd.Cmd [ Line Str ] } ) as Update
    }
mainForHost = { init, update }
