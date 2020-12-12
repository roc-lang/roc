interface Cmd
    exposes [ Cmd, none, map, putLine ]
    imports [ Effect ]

Cmd a : Effect.Effect a

none : Cmd {}
none = Effect.always {}

# getLine = Effect.after Effect.getLine always

putLine : Str -> Cmd {}
putLine = \line -> Effect.putLine line 

map : Cmd a, (a -> b) -> Cmd b
map = \cmd, transform -> Effect.map cmd transform
