interface Arg
    exposes [
        Parser,
        succeed,
        parse,
    ]
    imports []

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    # Default (Parser a) a,
    Lazy ({} -> a)
]

OptionStr : [Some Str, NotProvided]

Config : {
    long : Str,
    short : OptionStr,
    help : OptionStr,
}

Help : {
    configs : List Config
}

succeed : a -> Parser a
succeed = \val -> @Parser (Succeed val)

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

                    Arg config2 run2 ->
                        # Parse first the one and then the other.
                        Arg config2 \args ->
                            when run args is
                                Ok fn -> run2 args |> Result.map fn
                                Err err -> Err err

            Lazy thunk ->
                fn = thunk {}

                when parser is
                    Succeed a ->
                        Lazy \{} -> fn a

                    Lazy innerThunk ->
                        Lazy \{} -> fn (innerThunk {})

                    # Default parser2 defaultVal ->
                    #     parser2
                    #     |> andMap (@Parser mapper)
                    #     |> Default (fn defaultVal)

                    Arg config run ->
                        Arg config \args ->
                            run args
                            |> Result.map fn

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
