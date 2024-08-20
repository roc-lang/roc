app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}

import pf.Stdout
import pf.Task exposing [Task]

main =
    file = strParam { name: "file" }
    argParser =
        { cliBuild <-
            file,
            count: numParam { name: "count" },
            doubled: numParam { name: "doubled" }
            |> cliMap \d -> d * 2,
        }

    args = ["parse-args", "file.txt", "5", "7"]
    when argParser |> parseArgs args is
        Ok data -> Stdout.line "Success: $(Inspect.toStr data)"
        Err (FailedToParse message) -> Stdout.line "Failed: $(message)"

ArgParseErr : [NoMoreArgs, InvalidParam ParamConfig]

ParamConfig : {
    name : Str,
    type : [Num, Str],
}

ArgParser out : {
    params : List ParamConfig,
    parser : List Str -> Result (out, List Str) ArgParseErr,
}

strParam : { name : Str } -> ArgParser Str
strParam = \{ name } ->
    parser = \args ->
        when args is
            [] -> Err NoMoreArgs
            [first, .. as rest] -> Ok (first, rest)

    { params: [{ name, type: Str }], parser }

numParam : { name : Str } -> ArgParser U64
numParam = \{ name } ->
    param = { name, type: Num }
    parser = \args ->
        when args is
            [] -> Err NoMoreArgs
            [first, .. as rest] ->
                when Str.toU64 first is
                    Ok num -> Ok (num, rest)
                    Err InvalidNumStr -> Err (InvalidParam param)

    { params: [param], parser }

cliMap : ArgParser a, (a -> b) -> ArgParser b
cliMap = \{ params, parser }, mapper ->
    mappedParser = \args ->
        (data, afterData) = parser? args

        Ok (mapper data, afterData)

    {
        params,
        parser: mappedParser,
    }

cliBuild : ArgParser a, ArgParser b, (a, b -> c) -> ArgParser c
cliBuild = \firstWeaver, secondWeaver, combine ->
    allParams = List.concat firstWeaver.params secondWeaver.params
    combinedParser = \args ->
        (firstValue, afterFirst) = firstWeaver.parser? args
        (secondValue, afterSecond) = secondWeaver.parser? afterFirst

        Ok (combine firstValue secondValue, afterSecond)

    { params: allParams, parser: combinedParser }

parseArgs : ArgParser a, List Str -> Result a [FailedToParse Str]
parseArgs = \{ params: _, parser }, args ->
    when parser (List.dropFirst args 1) is
        Ok (data, []) -> Ok data
        Ok (_data, extraArgs) -> Err (FailedToParse "Got $(List.len extraArgs |> Inspect.toStr) extra args")
        Err NoMoreArgs -> Err (FailedToParse "I needed more args")
        Err (InvalidParam param) -> Err (FailedToParse "Parameter '$(param.name)' needed a $(Inspect.toStr param.type)")
