hosted Host
    exposes [openFile!, closeFile!, getFileLine!, getFileBytes!, putLine!, putRaw!, getLine!, getChar!]
    imports []

openFile! : Str => U64

closeFile! : U64 => {}

getFileLine! : U64 => Str

getFileBytes! : U64 => List U8

putLine! : Str => {}

putRaw! : Str => {}

getLine! : {} => Str

getChar! : {} => U8
