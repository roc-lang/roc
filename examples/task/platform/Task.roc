interface Task
    exposes [ Task, succeed, fail, after, map ]
    imports [ Effect.{ Effect } ]


Task ok err : Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    Effect.always (Ok val)


fail : err -> Task * err
fail = \val ->
    Effect.always (Err val)


after : Task a err, (a -> Task b err) -> Task b err
after = \effect, transform ->
    Effect.after effect, \result ->
        when result is
            Ok a -> transform a
            Err err -> Task.fail err


map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    Effect.after effect, \result ->
        when result is
            Ok a -> Task.succeed (transform a)
            Err err -> Task.fail err
