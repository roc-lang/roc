app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    WithConfig (Parser a) Config,
    # Default (Parser a) a,
    Lazy ({} -> a)
]

Config : {
    long : Str,
    short : Str,
    help : Str,
}

Help : {
    configs : List Config
}

toHelp : Parser * -> Help
toHelp = \parser -> #toHelpHelp parser []
    { configs: [] }

# toHelpHelp : Parser *, List Config -> Help
# toHelpHelp = \@Parser parser, configs ->
#     when parser is
#         Succeed _ -> { configs }
#         Arg config _ -> { configs: List.append configs config }
#         Default { parser: inner } -> { configs: List.append configs (toHelpHelp inner configs) }
#         Both inner1 inner2 ->
#             help1 = toHelpHelp inner1 configs
#             toHelpHelp inner1 help1.configs

expect
    parser = argBool { help: "blah", long: "foo", short: "F" }

    parse parser ["foo"] == Ok True

argStr : Config -> Parser Str
argStr = \config ->
    fn = \args ->
        { long, short ? "" } = config

        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg -> Ok foundArg

    @Parser (Arg config fn)

findOneArg : Str, Str, List Str -> Result Str [NotFound]*
findOneArg = \long, short, args ->
    longArg = "--\(long)"
    shortArg = "-\(short)"

    # TODO allow = as well, etc.
    result = List.findFirstIndex args \arg ->
        arg == longArg || arg == shortArg

    when result is
        Ok index ->
            # Return the next arg after the given one
            List.get args (index + 1)
            |> Result.mapErr \_ -> NotFound

        Err NotFound -> Err NotFound

andMap : Parser a, Parser (a -> b) -> Parser b
andMap = \@Parser parser, @Parser mapper ->
    unwrapped =
        when mapper is
            Succeed fn ->
                when parser is
                    Succeed a ->
                        Lazy \{} -> fn a

                    Lazy thunk ->
                        Lazy \{} -> fn (thunk {})

                    WithConfig parser2 config ->
                        parser2
                        |> andMap (@Parser mapper)
                        |> WithConfig config

                    # Default parser2 defaultVal ->
                    #     parser2
                    #     |> andMap (@Parser mapper)
                    #     |> Default (fn defaultVal)

                    Arg config run ->
                        Arg config \args ->
                            run args
                            |> Result.map fn

            Arg config run ->
                when parser is
                    Succeed a ->
                        Arg config \args ->
                            when run args is
                                Ok fn -> Ok (fn a)
                                Err err -> Err err

                    Lazy thunk ->
                        Arg config \args ->
                            when run args is
                                Ok fn -> Ok (fn (thunk {}))
                                Err err -> Err err

                    # Default parser2 defaultVal ->

                    WithConfig parser2 config2 ->
                        parser2
                        |> andMap (@Parser mapper)
                        |> WithConfig config2

                    Arg config2 run2 ->
                        # Parse first the one and then the other.
                        combinedParser = Arg config2 \args ->
                            when run args is
                                Ok fn -> run2 args |> Result.map fn
                                Err err -> Err err

                        # Store the extra config.
                        @Parser combinedParser
                        |> WithConfig config

            Lazy thunk ->
                fn = thunk {}

                when parser is
                    Succeed a ->
                        Lazy \{} -> fn a

                    Lazy innerThunk ->
                        Lazy \{} -> fn (innerThunk {})

                    WithConfig parser2 config ->
                        parser2
                        |> andMap (@Parser mapper)
                        |> WithConfig config

                    # Default parser2 defaultVal ->
                    #     parser2
                    #     |> andMap (@Parser mapper)
                    #     |> Default (fn defaultVal)

                    Arg config run ->
                        Arg config \args ->
                            run args
                            |> Result.map fn

            WithConfig mapper2 config ->
                @Parser parser
                |> andMap mapper2
                |> WithConfig config

    @Parser unwrapped

parse : Parser a, List Str -> Result a [MissingRequiredArg, WrongType]*
parse = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Arg _ run ->
            when run args is
                Ok val -> Ok val
                Err NotFound -> Err MissingRequiredArg
                Err WrongType -> Err WrongType

        # Default parser2 defaultVal ->
        #     parse parser2 args
        #     |> Result.withDefault defaultVal
        #     |> Ok

        Lazy thunk -> Ok (thunk {})
        WithConfig parser2 config ->
            parse parser2 args

argBool : Config -> Parser Bool
argBool = \config ->
    fn = \args ->
        { long, short } = config

        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg ->
                if foundArg == "true" then
                    Ok True
                else if foundArg == "false" then
                    Ok False
                else
                    Err WrongType

    @Parser (Arg config fn)


main =
    parser = argBool { long: "foo", short: "F", help: "blah" }

    if parse parser ["--foo", "true"] == Ok True then
        "yep!\n\n"
    else
        "nope!\n\n"
