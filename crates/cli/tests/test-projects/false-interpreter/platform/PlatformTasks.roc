hosted PlatformTasks
    exposes [openFile, closeFile, withFileOpen, getFileLine, getFileBytes, putLine, putRaw, getLine, getChar]
    imports []

openFile : Str -> Task U64 {}

closeFile : U64 -> Task {} {}

withFileOpen : Str, (U64 -> Task ok err) -> Task {} {}

getFileLine : U64 -> Task Str {}

getFileBytes : U64 -> Task (List U8) {}

putLine : Str -> Task {} {}

putRaw : Str -> Task {} {}

getLine : Task Str {}

getChar : Task U8 {}
