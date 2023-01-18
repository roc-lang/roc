app "example"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.1.2/3bKbbmgtIfOyC6FviJ9o8F8xqKutmXgjCJx3bMfVTSo.tar.br",
        parser: "../package/main.roc",  
    }
    imports [
        cli.Stdout,
        parser.ParserCore.{ Parser, buildPrimitiveParser, many },
        parser.ParserStr.{ parseStr },
    ]
    provides [ main ] to cli

main = 
    Stdout.line "Hello"

Letter : [A, B, C, Other]

letterParser : Parser (List U8) Letter
letterParser =
    input <- buildPrimitiveParser

    valResult = when input is
        [] -> Err (ParsingFailure "Nothing to parse")
        ['A', .. ] -> Ok A
        ['B', .. ] -> Ok B
        ['C', .. ] -> Ok C
        _ -> Ok Other

    valResult
    |> Result.map \val -> { val, input : List.dropFirst input }

expect
    input = "B"
    parser = letterParser
    result = parseStr parser input
    result == Ok B 

expect
    input = "BCXA"
    parser = many letterParser
    result = parseStr parser input
    result == Ok [ B, C, Other, A ]