interface UrlParser
    exposes [ str, parse ]
    imports [ fx.Effect, Task.{ Task } ]


# TODO make this an actual parser!
UrlParser a : [ @UrlParser a ]


str : UrlParser Str
str = @UrlParser ""

parse : Str, UrlParser a -> Result a [ DidNotMatch ]*
parse = \url, @UrlParser parser ->
    parser # TODO make this an actual parser!
