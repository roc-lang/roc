interface Task
    exposes [ Task, succeed, fail, after, map, putLine, getInt ]
    imports [ fx.Effect ]


Task ok err : Effect.Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)


fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)

after : Task a err, (a -> Task b err) -> Task b err
after = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> transform a
            Err err -> Task.fail err

map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> Task.succeed (transform a)
            Err err -> Effect.always (Err err) # Task.fail err  does not work. WEIRD!

putLine : Str -> Task {} *
putLine = \line -> Effect.map (Effect.putLine line) (\_ -> Ok {})

getInt : Task I64 []
getInt =
    Effect.after Effect.getInt \{ isError, value, errorCode } ->
        when isError is
            True ->
                when errorCode is
                    # A -> Task.fail InvalidCharacter
                    # B -> Task.fail IOError
                    _ -> Task.succeed -1

            False ->
                Task.succeed value
