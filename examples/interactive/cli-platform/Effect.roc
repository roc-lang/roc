hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, getLine, sendRequest]
    imports [InternalHttp.{ Request, Response }]
    generates Effect with [after, map, always, forever, loop]

putLine : Str -> Effect {}

getLine : Effect Str

sendRequest : Box Request -> Effect Response
