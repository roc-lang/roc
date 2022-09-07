interface Arg
    exposes [
        Parser,
        succeed,
        parse,
        toHelp,
    ]
    imports []

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    WithConfig (Parser a) Config,
    # Default (Parser a) a,
    Lazy ({} -> a),
]

OptionStr : [Some Str, NotProvided]

Config : {
    long : Str,
    short : OptionStr,
    help : OptionStr,
}

Help : {
    configs : List Config,
}

succeed : a -> Parser a
succeed = \val -> @Parser (Succeed val)

toHelp : Parser * -> Help
toHelp = \parser ->
    configs = toHelpHelper parser []

    { configs }

toHelpHelper : Parser *, List Config -> List Config
toHelpHelper = \@Parser parser, configs ->
    when parser is
        Succeed _ -> configs
        Lazy _ -> configs
        WithConfig innerParser config -> toHelpHelper innerParser (List.append configs config)
        Arg config _ -> List.append configs config

findOneArg : Str, OptionStr, List Str -> Result Str [NotFound]*
findOneArg = \long, optShort, args ->
    argMatches = \arg ->
        if arg == "--\(long)" then
            True
        else
            when optShort is
                Some short -> arg == "-\(short)"
                NotProvided -> False

    # TODO allow = as well, etc.
    result = List.findFirstIndex args argMatches

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
        WithConfig parser2 _config ->
            parse parser2 args

argBool : Config -> Parser Bool
argBool = \config ->
    fn = \args ->
        { long, short } = config

        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok "true" -> Ok True
            Ok "false" -> Ok False
            Ok _ -> Err WrongType

    @Parser (Arg config fn)

argStr : Config -> Parser Str
argStr = \config ->
    fn = \args ->
        { long, short } = config

        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg -> Ok foundArg

    @Parser (Arg config fn)

apply = \arg1, arg2 -> andMap arg2 arg1

# bool undashed long argument is missing
expect
    parser = argBool { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["foo"] == Err MissingRequiredArg

# bool dashed long argument without value is missing
expect
    parser = argBool { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo"] == Err MissingRequiredArg

# bool dashed long argument with value is determined true
expect
    parser = argBool { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo", "true"] == Ok True

# bool dashed long argument with value is determined false
expect
    parser = argBool { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo", "false"] == Ok False

# bool dashed long argument with value is determined wrong type
expect
    parser = argBool { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo", "not-a-bool"] == Err WrongType

# string dashed long argument without value is missing
expect
    parser = argStr { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo"] == Err MissingRequiredArg

# string dashed long argument with value is determined
expect
    parser = argStr { long: "foo", help: NotProvided, short: NotProvided }

    parse parser ["--foo", "itsme"] == Ok "itsme"

# two string parsers complete cases
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo) bar: \(bar)")
        |> apply (argStr { long: "foo", short: NotProvided, help: NotProvided })
        |> apply (argStr { long: "bar", short: NotProvided, help: NotProvided })

    cases = [
        ["--foo", "true", "--bar", "baz"],
        ["--bar", "baz", "--foo", "true"],
        ["--foo", "true", "--bar", "baz", "--other", "something"],
    ]

    List.all cases \args -> parse parser args == Ok "foo: true bar: baz"

# string and bool parsers build help
expect
    parser =
        succeed (\foo -> \bar -> \_bool -> "foo: \(foo) bar: \(bar)")
        |> apply (argStr { long: "foo", short: NotProvided, help: NotProvided })
        |> apply (argStr { long: "bar", short: NotProvided, help: NotProvided })
        |> apply (argBool { long: "bool", short: NotProvided, help: NotProvided })

    toHelp parser
    == {
        configs: [
            { long: "foo", short: NotProvided, help: NotProvided },
            { long: "bar", short: NotProvided, help: NotProvided },
            { long: "bool", short: NotProvided, help: NotProvided },
        ],
    }
