hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, getLine, writeFile]
    imports [Path]
    generates Effect with [after, map, always, forever, loop]

writeUtf8 : Path, Str -> Effect {}

putLine : Str -> Effect {}

getLine : Effect Str
