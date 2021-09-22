interface UrlParser
    exposes [ Url, UrlParser, str, parse ]
    imports [ fx.Effect, Task.{ Task } ]

Url : Str

# TODO make this an actual parser!
UrlParser a : [ @UrlParser a ]

str : UrlParser Url
str = @UrlParser ""

parse : Url, UrlParser a -> Result a [ DidNotMatch ]*
parse = \url, @UrlParser parser ->
    Ok parser # TODO make this an actual parser!
