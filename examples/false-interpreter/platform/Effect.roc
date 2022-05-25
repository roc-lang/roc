hosted Effect
    exposes [Effect, after, map, always, forever, loop, openFile, closeFile, withFileOpen, getFileLine, getFileBytes, putLine, putRaw, getLine, getChar]
    imports []
    generates Effect with [after, map, always, forever, loop]

openFile : Str -> Effect U64

closeFile : U64 -> Effect {}

withFileOpen : Str, (U64 -> Effect (Result ok err)) -> Effect {}

getFileLine : U64 -> Effect Str

getFileBytes : U64 -> Effect (List U8)

putLine : Str -> Effect {}

putRaw : Str -> Effect {}

getLine : Effect Str

getChar : Effect U8
