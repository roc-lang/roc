hosted Effect
    exposes [
        Effect,
        after,
        args,
        map,
        always,
        forever,
        loop,
        dirList,
        envDict,
        envVar,
        cwd,
        setCwd,
        exePath,
        stdoutLine,
        stdoutWrite,
        stderrLine,
        stderrWrite,
        stdinLine,
        sendRequest,
        fileReadBytes,
        fileDelete,
        fileWriteUtf8,
        fileWriteBytes,
        processExit,
    ]
    imports [InternalHttp.{ Request, Response }, InternalFile, InternalDir]
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}
stdoutWrite : Str -> Effect {}
stderrLine : Str -> Effect {}
stderrWrite : Str -> Effect {}
stdinLine : Effect Str

fileWriteBytes : List U8, List U8 -> Effect (Result {} InternalFile.WriteErr)
fileWriteUtf8 : List U8, Str -> Effect (Result {} InternalFile.WriteErr)
fileDelete : List U8 -> Effect (Result {} InternalFile.WriteErr)
fileReadBytes : List U8 -> Effect (Result (List U8) InternalFile.ReadErr)
dirList : List U8 -> Effect (Result (List (List U8)) InternalDir.ReadErr)
envDict : Effect (Dict Str Str)
envVar : Str -> Effect (Result Str {})
exePath : Effect (Result (List U8) {})
setCwd : List U8 -> Effect (Result {} {})

processExit : U8 -> Effect {}

# If we encounter a Unicode error in any of the args, it will be replaced with
# the Unicode replacement char where necessary.
args : Effect (List Str)

cwd : Effect (List U8)

sendRequest : Box Request -> Effect Response
