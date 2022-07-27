app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    WithConfig Config (Parser a),
    Default { parser : Parser a, default : a },
    Lazy ({} -> a)
]

Config : {
    long : Str,
    short ? Str,
    help ? Str,
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

parse : Parser a, List Str -> Result a [MissingRequiredArg, WrongType]*
parse = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Arg _ run ->
            when run args is
                Ok val -> Ok val
                Err NotFound -> Err MissingRequiredArg
                Err WrongType -> Err WrongType

        Default opts ->
            parse opts.parser args
            |> Result.withDefault opts.default
            |> Ok

        Lazy thunk -> Ok (thunk {})

argBool : Config -> Parser Bool
argBool = \config ->
    fn = \args ->
        { long, short ? "" } = config

        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg ->
                when foundArg is
                    "true" -> Ok True
                    "false" -> Ok False
                    _ -> Err WrongType

    @Parser (Arg config fn)

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
    # TODO use List.firstIndex to find the first index of the arg,
    # then return the one after it (if it exists).
    # TODO allow = as well, etc.
    Err NotFound

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

                    WithConfig config parser2 ->
                        WithConfig config (andMap parser2 (@Parser mapper))

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

                    WithConfig config2 parser2 ->
                        WithConfig config2 (andMap parser2 (@Parser mapper))

                    Arg config2 run2 ->
                        # Parses first the one and then the other.
                        combinedParser = Arg config2 \args ->
                            when run args is
                                Ok fn -> run2 args |> Result.map fn
                                Err err -> Err err

                        # Stores the extra config
                        WithConfig config (@Parser combinedParser)

            Lazy thunk ->
                when parser is
                    Succeed a ->
                        Lazy \{} -> (thunk {}) a

                    Lazy innerThunk ->
                        Lazy \{} -> (thunk {}) (innerThunk {})

    @Parser unwrapped

main = "Hello, World!\n"
