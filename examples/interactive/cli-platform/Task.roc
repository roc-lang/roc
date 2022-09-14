interface Task
    exposes [Task, succeed, fail, await, map, mapFail, onFail, attempt, forever, loop]
    imports [Effect, InternalTask]

Task ok err fx : InternalTask.Task ok err fx

forever : Task val err fx -> Task * err fx
forever = \task ->
    looper = \{} ->
        task
        |> InternalTask.toEffect
        |> Effect.map
            \res ->
                when res is
                    Ok _ -> Step {}
                    Err e -> Done (Err e)

    Effect.loop {} looper
    |> InternalTask.fromEffect

loop : state, (state -> Task [Step state, Done done] err fx) -> Task done err fx
loop = \state, step ->
    looper = \current ->
        step current
        |> InternalTask.toEffect
        |> Effect.map
            \res ->
                when res is
                    Ok (Step newState) -> Step newState
                    Ok (Done result) -> Done (Ok result)
                    Err e -> Done (Err e)

    Effect.loop state looper
    |> InternalTask.fromEffect

succeed : ok -> Task ok * *
succeed = \ok -> InternalTask.succeed ok

fail : err -> Task * err *
fail = \err -> InternalTask.fail err

attempt : Task a b fx, (Result a b -> Task c d fx) -> Task c d fx
attempt = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok ok -> transform (Ok ok) |> InternalTask.toEffect
                Err err -> transform (Err err) |> InternalTask.toEffect

    InternalTask.fromEffect effect

await : Task a err fx, (a -> Task b err fx) -> Task b err fx
await = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> transform a |> InternalTask.toEffect
                Err err -> Task.fail err |> InternalTask.toEffect

    InternalTask.fromEffect effect

onFail : Task ok a fx, (a -> Task ok b fx) -> Task ok b fx
onFail = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> Task.succeed a |> InternalTask.toEffect
                Err err -> transform err |> InternalTask.toEffect

    InternalTask.fromEffect effect

map : Task a err fx, (a -> b) -> Task b err fx
map = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok ok -> Task.succeed (transform ok) |> InternalTask.toEffect
                Err err -> Task.fail err |> InternalTask.toEffect

    InternalTask.fromEffect effect

mapFail : Task ok a fx, (a -> b) -> Task ok b fx
mapFail = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok ok -> Task.succeed ok |> InternalTask.toEffect
                Err err -> Task.fail (transform err) |> InternalTask.toEffect

    InternalTask.fromEffect effect
