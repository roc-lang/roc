interface Arg
    exposes [
        Parser,

        parse,
        toHelp,

        succeed,
        bool,
        str,

        apply,
    ]
    imports []

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    WithConfig (Parser a) Config,
    # Default (Parser a) a,
    Lazy ({} -> a),
]

ParseError a : [
    MissingRequiredArg Str,
    WrongType {
        arg: Str,
        expected: Type,
    }
]a

Type : [
    Str,
    Bool,
]

Help : {
    configs : List Config,
}

OptionStr : [Some Str, NotProvided]

Config : {
    long : Str,
    short : OptionStr,
    help : OptionStr,
    type : Type,
}

OptConfig : {
    long : Str,
    short ?OptionStr,
    help ?OptionStr,
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

parse : Parser a, List Str -> Result a (ParseError*)
parse = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Arg {long, type} run ->
            when run args is
                Ok val -> Ok val
                Err NotFound -> Err (MissingRequiredArg long)
                Err WrongType -> Err (WrongType { arg: long, expected: type })

        # Default parser2 defaultVal ->
        #     parse parser2 args
        #     |> Result.withDefault defaultVal
        #     |> Ok
        Lazy thunk -> Ok (thunk {})
        WithConfig parser2 _config ->
            parse parser2 args

## Like [parse], runs a parser to completion on a list of arguments.
## If the parser fails, a formatted error and help message is returned.
parseFormatted : Parser a, List Str -> Result a Str
parseFormatted = \parser, args ->
    when parse parser args is
        Ok a -> Ok a
        Err e ->
            Str.concat (Str.concat (formatHelp parser) "\n\n") (formatError e)

bool : _ -> Parser Bool # TODO: panics if OptConfig annotated
bool = \{ long, short ? NotProvided, help ? NotProvided } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok "true" -> Ok True
            Ok "false" -> Ok False
            Ok _ -> Err WrongType

    @Parser (Arg { long, short, help, type: Bool } fn)

str : _ -> Parser Str # TODO: panics if OptConfig annotated
str = \{ long, short ? NotProvided, help ? NotProvided } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg -> Ok foundArg

    @Parser (Arg { long, short, help, type: Str } fn)

apply = \arg1, arg2 -> andMap arg2 arg1

# bool undashed long argument is missing
expect
    parser = bool { long: "foo" }

    parse parser ["foo"] == Err (MissingRequiredArg "foo")

# bool dashed long argument without value is missing
expect
    parser = bool { long: "foo" }

    parse parser ["--foo"] == Err (MissingRequiredArg "foo")

# bool dashed long argument with value is determined true
expect
    parser = bool { long: "foo" }

    parse parser ["--foo", "true"] == Ok True

# bool dashed long argument with value is determined false
expect
    parser = bool { long: "foo" }

    parse parser ["--foo", "false"] == Ok False

# bool dashed long argument with value is determined wrong type
expect
    parser = bool { long: "foo" }

    parse parser ["--foo", "not-a-bool"] == Err (WrongType { arg: "foo", expected: Bool })

# bool dashed short argument with value is determined true
expect
    parser = bool { long: "foo", short: Some "F" }

    parse parser ["-F", "true"] == Ok True

# bool dashed short argument with value is determined false
expect
    parser = bool { long: "foo", short: Some "F" }

    parse parser ["-F", "false"] == Ok False

# bool dashed short argument with value is determined wrong type
expect
    parser = bool { long: "foo", short: Some "F" }

    parse parser ["-F", "not-a-bool"] == Err (WrongType { arg: "foo", expected: Bool })

# string dashed long argument without value is missing
expect
    parser = str { long: "foo" }

    parse parser ["--foo"] == Err (MissingRequiredArg "foo")

# string dashed long argument with value is determined
expect
    parser = str { long: "foo" }

    parse parser ["--foo", "itsme"] == Ok "itsme"

# string dashed short argument without value is missing
expect
    parser = str { long: "foo", short: Some "F" }

    parse parser ["-F"] == Err (MissingRequiredArg "foo")

# string dashed short argument with value is determined
expect
    parser = str { long: "foo", short: Some "F" }

    parse parser ["-F", "itsme"] == Ok "itsme"

# two string parsers complete cases
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo) bar: \(bar)")
        |> apply (str { long: "foo" })
        |> apply (str { long: "bar" })

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
        |> apply (str { long: "foo", help: Some "the foo flag" })
        |> apply (str { long: "bar", short: Some "B" })
        |> apply (bool { long: "bool" })

    toHelp parser
    == {
        configs: [
            { long: "foo", short: NotProvided, help: Some "the foo flag", type: Str },
            { long: "bar", short: Some "B", help: NotProvided, type: Str },
            { long: "bool", short: NotProvided, help: NotProvided, type: Bool },
        ],
    }
