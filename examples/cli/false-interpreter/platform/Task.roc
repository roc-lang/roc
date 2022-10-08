interface Task
    exposes [Task, succeed, fail, await, map, onFail, attempt, fromResult, loop]
    imports [pf.Effect]

Task ok err : Effect.Effect (Result ok err)

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

fromResult : Result a e -> Task a e
fromResult = \result ->
    when result is
        Ok a -> succeed a
        Err e -> fail e

attempt : Task a b, (Result a b -> Task c d) -> Task c d
attempt = \effect, transform ->
    Effect.after
        effect
        \result ->
            when result is
                Ok ok -> transform (Ok ok)
                Err err -> transform (Err err)

await : Task a err, (a -> Task b err) -> Task b err
await = \effect, transform ->
    Effect.after
        effect
        \result ->
            when result is
                Ok a -> transform a
                Err err -> Task.fail err

onFail : Task ok a, (a -> Task ok b) -> Task ok b
onFail = \effect, transform ->
    Effect.after
        effect
        \result ->
            when result is
                Ok a -> Task.succeed a
                Err err -> transform err

map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.after
        effect
        \result ->
            when result is
                Ok a -> Task.succeed (transform a)
                Err err -> Task.fail err
