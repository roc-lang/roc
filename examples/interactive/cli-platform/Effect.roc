hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, getLine, sendRequest]
    imports [Http.{ Request, Reponse }]
    generates Effect with [after, map, always, forever, loop]

putLine : Str -> Effect {}

getLine : Effect Str

sendRequest : Request -> Effect Reponse
