interface InternalTask
    exposes [Task, fromEffect, toEffect, succeed, fail]
    imports [Effect.{ Effect }]

Task ok err := Effect (Result ok err)

succeed : ok -> Task ok *
succeed = \ok -> @Task (Effect.always (Ok ok))

fail : err -> Task * err
fail = \err -> @Task (Effect.always (Err err))

fromEffect : Effect (Result ok err) -> Task ok err
fromEffect = \effect -> @Task effect

toEffect : Task ok err -> Effect (Result ok err)
toEffect = \@Task effect -> effect
