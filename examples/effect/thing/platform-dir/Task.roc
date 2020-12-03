interface Task
    exposes [ Task, putLine, after ]
    imports [ Effect ]

Task a : Effect.Effect a

putLine = Effect.putLine

after = Effect.after
