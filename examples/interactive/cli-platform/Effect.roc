hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, errLine, getLine, writeUtf8, writeBytes]
    imports [Path.{ Path }]
    generates Effect with [after, map, always, forever, loop]

writeUtf8 : Path, Str -> Effect {}
writeBytes : Path, List U8 -> Effect {}

putLine : Str -> Effect {}

errLine : Str -> Effect {}

getLine : Effect Str
