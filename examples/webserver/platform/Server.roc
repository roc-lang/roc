interface Server
    exposes [ Server, init ]
    imports [ Route.{ Handler }, UrlParser.{ Url } ]


Server : [ @Server { port : U32, handlers : List Handler } ]


init : { port : U32, handlers : List Handler } -> Server
init = \{ port, handlers } -> @Server { port, handlers }
