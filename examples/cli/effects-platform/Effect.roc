hosted Effect
    exposes [Effect, after, map, always, forever, putLine, getLine]
    generates Effect with [after, map, always, forever]

putLine : Str -> Effect {}

getLine : Effect Str
