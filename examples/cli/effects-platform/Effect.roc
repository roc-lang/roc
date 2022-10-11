hosted Effect
    exposes [Effect, after, map, always, forever, putLine, getLine]
    imports []
    generates Effect with [after, map, always, forever]

putLine : Str -> Effect {}

getLine : Effect Str
