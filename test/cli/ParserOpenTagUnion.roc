ParserOpenTagUnion :: [].{}

parse_open : Str -> Try([Friendly, ..tags], [InvalidJson(Str)])
parse_open = |json| Json.parse(json)

main = parse_open("\"Friendly\"")
