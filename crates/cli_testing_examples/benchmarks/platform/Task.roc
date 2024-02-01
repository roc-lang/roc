interface Task
    exposes [Task, succeed, fail, after, map, putLine, putInt, getInt, forever, loop, attempt]

import pf.Effect

Task ok err : Effect.Effect (Result ok err)

forever : Task val err -> Task * err
forever = \task ->
    looper = \{} ->
        task
        |> Effect.map
            \res ->
                when res is
                    Ok _ -> Step {}
                    Err e -> Done (Err e)

    Effect.loop {} looper

loop : state, (state -> Task [Step state, Done done] err) -> Task done err
loop = \state, step ->
    looper = \current ->
        step current
        |> Effect.map
            \res ->
                when res is
                    Ok (Step newState) -> Step newState
                    Ok (Done result) -> Done (Ok result)
                    Err e -> Done (Err e)

    Effect.loop state looper

succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)

fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)

after : Task a err, (a -> Task b err) -> Task b err
after = \effect, transform ->
    Effect.after
        effect
        \result ->
            when result is
                Ok a -> transform a
                Err err -> Task.fail err

attempt : Task a b, (Result a b -> Task c d) -> Task c d
attempt = \task, transform ->
    Effect.after
        task
        \result ->
            when result is
                Ok ok -> transform (Ok ok)
                Err err -> transform (Err err)

map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.map
        effect
        \result ->
            when result is
                Ok a -> Ok (transform a)
                Err err -> Err err

putLine : Str -> Task {} *
putLine = \line -> Effect.map (Effect.putLine line) (\_ -> Ok {})

putInt : I64 -> Task {} *
putInt = \line -> Effect.map (Effect.putInt line) (\_ -> Ok {})

getInt : Task I64 [GetIntError]
getInt =
    Effect.after
        Effect.getInt
        \{ isError, value } ->
            if
                isError
            then
                # TODO
                # when errorCode is
                #    # A -> Task.fail InvalidCharacter
                #    # B -> Task.fail IOError
                #    _ ->
                Task.fail GetIntError
            else
                Task.succeed value
