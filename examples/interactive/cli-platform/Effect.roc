hosted Effect
    exposes [
        Effect,
        after,
        map,
        always,
        forever,
        loop,
        dirList,
        cwd,
        stdoutLine,
        stderrLine,
        stdinLine,
        sendRequest,
        fileReadBytes,
        fileDelete,
        fileWriteUtf8,
        fileWriteBytes,
    ]
    imports [InternalHttp.{ Request, Response }, InternalFile, InternalDir]
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}
stderrLine : Str -> Effect {}
stdinLine : Effect Str

fileWriteBytes : List U8, List U8 -> Effect (Result {} InternalFile.WriteErr)
fileWriteUtf8 : List U8, Str -> Effect (Result {} InternalFile.WriteErr)
fileDelete : List U8 -> Effect (Result {} InternalFile.WriteErr)
fileReadBytes : List U8 -> Effect (Result (List U8) InternalFile.ReadErr)
dirList : List U8 -> Effect (Result (List (List U8)) InternalDir.ReadErr)

cwd : Effect (List U8)

sendRequest : Box Request -> Effect Response
