app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.5.2/9VrPjwfQQ1QeSL3CfmWr2Pr9DESdDIXy97pwpuq84Ck.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        parser.Core.{ Parser, buildPrimitiveParser, many },
        parser.String.{ parseStr },
    ]
    provides [main] to cli

main =
    lettersInput = "AAAiBByAABBwBtCCCiAyArBBx"
    ifLetterA = \l -> l == A
    when parseStr (many letterParser) lettersInput is
        Ok letters ->
            letters
            |> List.keepIf ifLetterA
            |> List.map \_ -> 1
            |> List.sum
            |> Num.toStr
            |> \countLetterA -> Stdout.line "I counted $(countLetterA) letter A's!"

        Err _ -> Stderr.line "Ooops, something went wrong parsing letters"

Letter : [A, B, C, Other]

letterParser : Parser (List U8) Letter
letterParser =
    input <- buildPrimitiveParser

    valResult =
        when input is
            [] -> Err (ParsingFailure "Nothing to parse")
            ['A', ..] -> Ok A
            ['B', ..] -> Ok B
            ['C', ..] -> Ok C
            _ -> Ok Other

    valResult
    |> Result.map \val -> { val, input: List.dropFirst input 1 }

expect
    input = "B"
    parser = letterParser
    result = parseStr parser input
    result == Ok B

expect
    input = "BCXA"
    parser = many letterParser
    result = parseStr parser input
    result == Ok [B, C, Other, A]
