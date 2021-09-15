interface Task
    exposes [ Task, succeed, fail, await, map, onFail, attempt, sequence ]
    imports [ fx.Effect ]


Task ok err : Effect.Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)


fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)

attempt : Task a b, (Result a b -> Task c d) -> Task c d
attempt = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok ok -> transform (Ok ok)
            Err err -> transform (Err err)

await : Task a err, (a -> Task b err) -> Task b err
await = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> transform a
            Err err -> Task.fail err

onFail : Task ok a, (a -> Task ok b) -> Task ok b
onFail = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> Task.succeed a
            Err err -> transform err

map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> Task.succeed (transform a)
            Err err -> Task.fail err

## Run each of the given tasks in sequence, and collect a list of their outputs.
## If any task fails, the entire task fails.
sequence : List (Task a err) -> Task (List a) err
sequence = \tasks ->
    # TODO create this list with a capacity of (List.len tasks)
    initialList = []

    step = \task, accumTask ->
        list <- await accumTask
        elem <- await task

        List.append list elem
            |> Task.succeed

    List.walk tasks step (Task.succeed initialList)
