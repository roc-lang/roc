platform "webserver-platform"
    requires {} { main : _ }
    exposes [Task, Http, Utc]
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \str -> main str
