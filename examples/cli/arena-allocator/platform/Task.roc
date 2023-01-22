interface Task
    exposes [ Task, succeed, fail, await, map, onFail, attempt, fromResult, useArenaAlloc ]
    imports [ fx.Effect ]


Task ok err : Effect.Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)


fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)

fromResult : Result a e -> Task a e
fromResult = \result ->
    when result is
        Ok a ->
            succeed a

        Err e ->
            fail e

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

useArenaAlloc : (U8 -> Task a err) -> Task a err
useArenaAlloc = \task ->
    # `n` is used to prevent `task` from running before `arenaStart`
    n <- Task.await arenaStart
    result <- Task.attempt (task n)
    {} <- Task.await arenaEnd

    Task.fromResult result

arenaStart : Task U8 *
arenaStart =
    Effect.after Effect.arenaStart Task.succeed

arenaEnd : Task {} *
arenaEnd =
    Effect.after Effect.arenaEnd Task.succeed
