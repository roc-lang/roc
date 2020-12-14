app "effect-example"
    packages { base: "platform" }
    imports [base.Cmd]
    provides [ main ] to base

Model : I64

Msg : [ Line Str ]

main = { init, update }

init : {} -> { model : Model, cmd : Cmd.Cmd Msg }
init = \{} -> 
    cmd = 
        Cmd.after (Cmd.putLine "Type a thing, and I'll say it back") \{} -> 
            Cmd.getLine (\l -> Line l) 

    { model: 42, cmd }


update : Msg, Model -> { model : Model, cmd : Cmd.Cmd Msg }
update = \msg, model -> 
    when msg is
        Line line -> 
            cmd = 
                Cmd.after (Cmd.putLine "You said:") \{} ->                             
                Cmd.after (Cmd.putLine line) \{} ->                             
                Cmd.after (Cmd.putLine "Type another thing, and I'll say it back") \{} ->                             
                Cmd.getLine (\l -> Line l) 
                    
            { model: model + 1, cmd }
