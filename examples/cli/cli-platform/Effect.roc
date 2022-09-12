hosted Effect
    exposes [
        Effect,
        after,
        map,
        always,
        forever,
        loop,
        stdoutLine,
        stderrLine,
        stdinLine,
        sendRequest,
        fileReadBytes,
        fileWriteUtf8,
        fileWriteBytes,
    ]
    imports [InternalHttp.{ Request, Response }, InternalFile]
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}
stderrLine : Str -> Effect {}
stdinLine : Effect Str

fileWriteBytes : List U8, List U8 -> Effect (Result {} InternalFile.WriteErr)
fileWriteUtf8 : List U8, Str -> Effect (Result {} InternalFile.WriteErr)
fileReadBytes : List U8 -> Effect (Result (List U8) InternalFile.ReadErr)

sendRequest : Box Request -> Effect Response
