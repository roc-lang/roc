hosted Effect
    exposes [
        Effect,
        after,
        args,
        map,
        always,
        forever,
        loop,
        envDict,
        envVar,
        stdoutLine,
        sendRequest,
    ]
    imports [InternalHttp.{ Request, Response }]
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}

envDict : Effect (Dict Str Str)
envVar : Str -> Effect (Result Str {})

# If we encounter a Unicode error in any of the args, it will be replaced with
# the Unicode replacement char where necessary.
args : Effect (List Str)

sendRequest : Box Request -> Effect Response
