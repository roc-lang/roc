platform "cli"
    requires {} { glue : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \input -> glue input
