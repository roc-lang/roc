interface Stdout
    exposes [ write ]
    imports [ Task.{ Task }, Effect.{ Effect } ]

write : Str -> Effect {} #Task {} *
write = \str -> Effect.putLine st
