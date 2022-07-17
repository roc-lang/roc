hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, errLine, getLine, writeUtf8, writeBytes, httpGetUtf8, envVarUtf8, sendRequest]
    imports [InternalHttp.{ Request, Response }, Path.{ Path }, Url.{ Url }]
    generates Effect with [after, map, always, forever, loop]

envVarUtf8 : Str -> Effect Str

httpGetUtf8 : Url -> Effect Str

writeUtf8 : Path, Str -> Effect {}
writeBytes : Path, List U8 -> Effect {}

putLine : Str -> Effect {}
errLine : Str -> Effect {}
getLine : Effect Str

sendRequest : Box Request -> Effect Response
