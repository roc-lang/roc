app "example"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.1.2/3bKbbmgtIfOyC6FviJ9o8F8xqKutmXgjCJx3bMfVTSo.tar.br",
        parser: "../package/main.roc",  
    }
    imports [
        cli.Stdout,
        parser.ParserCore.{ Parser, parsePartial, buildPrimitiveParser },
    ]
    provides [ main ] to cli

main = 
    Stdout.line "Hello"

Letter : [A, B, C, Other]

letterParser : Parser (List U8) Letter
letterParser =
    input <- buildPrimitiveParser

    val = when input is 
        ['A', .. ] -> A
        ['B', .. ] -> B
        ['C', .. ] -> C
        _ -> Other

    Ok { val, input : List.dropFirst input }    

expect
    input = [ 'B', 'C', 'X' ,'A' ]
    parser = letterParser
    result = parsePartial parser input
    result == Ok { val : B, input : ['C', 'X' ,'A'] }

# Currently hangs the compiler refer [Issue 4904](https://github.com/roc-lang/roc/issues/4904) 
# expect
#     input = [ 'B', 'C', 'X' ,'A' ]
#     parser = many letterParser
#     result = parsePartial parser input
#     result == Ok { val : [ B, C, Other, A ], input : [] }