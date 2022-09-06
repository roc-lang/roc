hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, getLine, sendRequest]
    imports [InternalHttp.{ Request, Response }]
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}
stderrLine : Str -> Effect {}
stdinLine : Effect Str

sendRequest : Box Request -> Effect Response
