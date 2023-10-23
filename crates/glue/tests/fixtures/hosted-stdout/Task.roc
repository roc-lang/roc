interface Task
    exposes [Task, succeed, fail, stdoutLine]
    imports [Effect]


Task ok err := Effect.Effect (Result ok err)

succeed : ok -> Task ok *
succeed = \ok -> @Task (Effect.always (Ok ok))

fail : err -> Task * err
fail = \err -> @Task (Effect.always (Err err))

stdoutLine : Str -> Task {} *
stdoutLine = \line -> @Task (Effect.stdoutLine "\(line)\n" |> Effect.map \_ -> Ok {})
