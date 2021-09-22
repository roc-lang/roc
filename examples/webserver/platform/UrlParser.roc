interface UrlParser
    exposes [ Url, UrlParser, str, parse ]
    imports [ fx.Effect, Task.{ Task } ]

Url : Str

# TODO make this an actual parser!
UrlParser a : [ @UrlParser (Str -> Result a [ DidNotMatch ]) ]

str : UrlParser Url
str = @UrlParser \url -> Ok (chompUntilSlash url)

parse : Url, UrlParser a -> Result a [ DidNotMatch ]*
parse = \url, @UrlParser parser ->
    when parser url is
        Ok x -> Ok x
        Err DidNotMatch -> Err DidNotMatch


chompUntilSlash : Str -> Str
chompUntilSlash = \string ->
    string
        |> Str.toUtf8
        |> keepWhile (\c -> c != 47)
        |> Str.fromUtf8
        |> Result.withDefault ""


keepWhile = \list, predicate ->
    walker = \element, accum ->
        if predicate element then
            Continue (List.append accum element)

        else
            Stop accum

    List.walkUntil list walker []
