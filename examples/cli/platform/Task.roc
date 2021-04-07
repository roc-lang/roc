interface Task
    exposes [ Task, succeed, fail, await, map ]
    imports [ fx.Effect ]


Task ok err : Effect.Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)


fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)


await : Task a err, (a -> Task b err) -> Task b err
await = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> transform a
            Err err -> Task.fail err

map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.after effect \result ->
        when result is
            Ok a -> Task.succeed (transform a)
            Err err -> Task.fail err
