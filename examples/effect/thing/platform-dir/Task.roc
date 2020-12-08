interface Task
    exposes [ Task, putLine, after ]
    imports [ Effect ]

Task a : Effect.Effect a

putLine = \line -> Effect.putLine line

after = Effect.after
