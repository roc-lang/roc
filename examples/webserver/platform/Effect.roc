hosted Effect
    exposes [
        Effect,
        after,
        map,
        always,
        forever,
        loop,
    ]
    imports [
        InternalHttp.{ Request, Response },
    ]
    generates Effect with [after, map, always, forever, loop]

sendRequest : Box Request -> Effect Response

posixTime : Effect U128
sleepMillis : U64 -> Effect {}
