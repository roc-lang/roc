interface Cmd
    exposes [ Cmd, none, map, putLine, getLine, always, after ]
    imports [ fx.Effect ]

Cmd a : Effect.Effect a

none : Cmd {}
none = Effect.always {}

always : {} -> Cmd {}
always = \x -> Effect.always x

getLine : (Str -> msg) -> Cmd msg
getLine = \toMsg ->
    Effect.map Effect.getLine toMsg

putLine : Str -> Cmd {}
putLine = \line -> Effect.putLine line

map : Cmd a, (a -> b) -> Cmd b
map = \cmd, transform -> Effect.map cmd transform

after : Cmd a, (a -> Cmd b) -> Cmd b
after = \cmd, transform -> Effect.after cmd transform
