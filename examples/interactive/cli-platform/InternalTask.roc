interface InternalTask
    exposes [Task, fromEffect, toEffect]
    imports [pf.Effect.{ Effect }]

Task ok err fx := Effect (Result ok err)

fromEffect : Effect (Result ok err) -> Task ok err *
fromEffect = \effect -> @Task effect

toEffect : Task ok err * -> Effect (Result ok err)
toEffect = \@Task effect -> effect
