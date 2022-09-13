interface Arg
    exposes [
        Parser,
        NamedParser,

        parse,
        toHelp,
        parseFormatted,

        succeed,
        bool,
        str,
        subCommand,
        choice,

        withParser,
        named,
    ]
    imports []

NamedParser a := {name: Str, parser: Parser a}

Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    # TODO: hiding the record behind an alias currently causes a panic
    SubCommand (List {
        name: Str,
        parser: Parser a,
    }),

    # Constructed during transformations of the above variants
    WithConfig (Parser a) Config,
    Lazy ({} -> a),
]

ParseError a : [
    ProgramNameNotProvided Str,
    MissingRequiredArg Str,
    WrongType {
        arg: Str,
        expected: Type,
    },
    SubCommandNotFound {
        choices: List Str,
    },
    IncorrectSubCommand {
        found: Str,
        choices: List Str,
    },
]a

Type : [
    Str,
    Bool,
]

Help : [
    SubCommands (List {name: Str, help: Help }),
    Config (List Config),
]

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
    toHelpHelper parser []

# TODO: check overflows when this annotation is included
# toHelpHelper : Parser *, List Config -> Help
toHelpHelper = \@Parser parser, configs ->
    when parser is
        Succeed _ -> Config configs
        Lazy _ -> Config configs
        WithConfig innerParser config ->
            toHelpHelper innerParser (List.append configs config)
        Arg config _ ->
            List.append configs config
            |> Config
        SubCommand commands ->
            List.map
                commands
                (\{name, parser: innerParser} -> {name, help: (toHelpHelper innerParser [])})
            |> SubCommands

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

#andMap : Parser a, Parser (a -> b) -> Parser b
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

                    Arg config run ->
                        Arg config \args ->
                            run args
                            |> Result.map fn

                    SubCommand cmds ->
                        (List.map
                            cmds
                            \{name, parser: parser2} ->
                                {name, parser: andMap parser2 (@Parser mapper)})
                        |> SubCommand

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

                    SubCommand cmds ->
                        # For each subcommand, first run the subcommand, then
                        # push the result through the arg parser.
                        (List.map
                            cmds
                            \{name, parser: parser2} ->
                                {name, parser: andMap parser2 (@Parser mapper)})
                        |> SubCommand

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

                    Arg config run ->
                        Arg config \args ->
                            run args
                            |> Result.map fn

                    SubCommand cmds ->
                        (List.map
                            cmds
                            \{name, parser: parser2} ->
                                {name, parser: andMap parser2 (@Parser mapper)})
                        |> SubCommand

            WithConfig mapper2 config ->
                @Parser parser
                |> andMap mapper2
                |> WithConfig config

            SubCommand cmds ->
                (List.map
                    cmds
                    \{name, parser: mapper2} ->
                        {name, parser: andMap (@Parser parser) mapper2})
                |> SubCommand


    @Parser unwrapped

named = \parser, name -> @NamedParser {name, parser}

# TODO panics in alias analysis when this annotation is included
#parse : NamedParser a, List Str -> Result a (ParseError*)
parse = \@NamedParser parser, args ->
    # By convention the first string in the arg list is the program name.
    if List.isEmpty args
    then Err (ProgramNameNotProvided parser.name)
    else parseHelp parser.parser (List.split args 1).others

parseHelp : Parser a, List Str -> Result a (ParseError*)
parseHelp = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Arg {long, type} run ->
            when run args is
                Ok val -> Ok val
                Err NotFound -> Err (MissingRequiredArg long)
                Err WrongType -> Err (WrongType { arg: long, expected: type })
        SubCommand cmds ->
            when List.get args 0 is
                Ok cmd ->
                    argsRest = (List.split args 1).others
                    state =
                        List.walkUntil
                            cmds
                            (Err {})
                            \st, {name, parser: subParser} ->
                                if cmd == name
                                then Break (Ok (parseHelp subParser argsRest))
                                else Continue st
                    when state is
                        Ok result -> result
                        Err {} -> Err (IncorrectSubCommand {found: cmd, choices: List.map cmds .name})
                Err OutOfBounds -> Err (SubCommandNotFound {choices: List.map cmds .name})

        Lazy thunk -> Ok (thunk {})
        WithConfig parser2 _config ->
            parseHelp parser2 args

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

subCommand : Str, Parser a -> {name: Str, parser: Parser a}
subCommand = \name, parser -> {name, parser}

choice : List {name: Str, parser: Parser a} -> Parser a
choice = \subCommands -> @Parser (SubCommand subCommands)

## Like [parse], runs a parser to completion on a list of arguments.
## If the parser fails, a formatted error and help message is returned.
# TODO: mono panics in the args example if the type annotation is included
#parseFormatted : NamedParser a, List Str -> Result a Str
parseFormatted = \@NamedParser parser, args ->
    Result.mapErr
        (parse (@NamedParser parser) args)
        \e ->
            Str.concat (Str.concat (formatHelp (@NamedParser parser)) "\n\n") (formatError e)

indent : Nat -> Str
indent = \n -> Str.repeat " " n

indentLevel : Nat
indentLevel = 4

# formatHelp : NamedParser a -> Str
formatHelp = \@NamedParser {name, parser} ->
    cmdHelp = toHelp parser

    fmtCmdHeading =
        when cmdHelp is
            SubCommands _ -> "COMMANDS:"
            Config _ -> "OPTIONS:"

    fmtCmdHelp = formatCmdHelp indentLevel cmdHelp

    """
    \(name)

    \(fmtCmdHeading)
    \(fmtCmdHelp)
    """

# formatCmdHelp : Nat, Help -> Str <- TODO: layout-gen panics when the following annotation is applied!
formatCmdHelp = \n, help ->
    when help is
        SubCommands cmds ->
            Str.joinWith
                (List.map cmds \subCmd -> formatSubCommand n subCmd)
                "\n\n"
        Config configs ->
            Str.joinWith (List.map configs \c -> formatConfig n c) "\n"

formatSubCommand = \n, {name, help} ->
    indented = indent n

    fmtHelp = formatCmdHelp (n + indentLevel) help

    "\(indented)\(name)\n\(fmtHelp)"

formatConfig : Nat, Config -> Str
formatConfig = \n, {long, short, help, type} ->
    indented = indent n

    formattedShort =
        when short is
            NotProvided -> ""
            Some s -> ", -\(s)"

    formattedType = formatType type

    formattedHelp =
        when help is
            NotProvided -> ""
            Some h -> "    \(h)"

    "\(indented)--\(long)\(formattedShort)\(formattedHelp)  (\(formattedType))"

formatType : Type -> Str
formatType = \type ->
    when type is
        Bool -> "bool"
        Str -> "string"

quote = \s -> "\"\(s)\""

formatError : ParseError [] -> Str
formatError = \err ->
    when err is
        ProgramNameNotProvided program ->
            "The program name \"\(program)\" was not probided as a first argument!"
        MissingRequiredArg arg ->
            "Argument `--\(arg)` is required but was not provided!"
        WrongType { arg, expected } ->
            formattedType = formatType expected
            "The argument `--\(arg)` expects a value of type \(formattedType)!"
        SubCommandNotFound { choices } ->
            fmtChoices =
                (List.map choices quote)
                |> Str.joinWith ", "

            """
            A subcommand was expected, but not found!
            The available subcommands are:
            \t\(fmtChoices)
            """
        IncorrectSubCommand { found, choices } ->
            fmtFound = quote found

            fmtChoices =
                (List.map choices quote)
                |> Str.joinWith ", "

            """
            The \(fmtFound) subcommand was found, but it's not expected in this context! 
            The available subcommands are:
            \t\(fmtChoices)
            """

withParser = \arg1, arg2 -> andMap arg2 arg1

# bool undashed long argument is missing
expect
    parser = bool { long: "foo" }

    parseHelp parser ["foo"] == Err (MissingRequiredArg "foo")

# bool dashed long argument without value is missing
expect
    parser = bool { long: "foo" }

    parseHelp parser ["--foo"] == Err (MissingRequiredArg "foo")

# bool dashed long argument with value is determined true
expect
    parser = bool { long: "foo" }

    parseHelp parser ["--foo", "true"] == Ok True

# bool dashed long argument with value is determined false
expect
    parser = bool { long: "foo" }

    parseHelp parser ["--foo", "false"] == Ok False

# bool dashed long argument with value is determined wrong type
expect
    parser = bool { long: "foo" }

    parseHelp parser ["--foo", "not-a-bool"] == Err (WrongType { arg: "foo", expected: Bool })

# bool dashed short argument with value is determined true
expect
    parser = bool { long: "foo", short: Some "F" }

    parseHelp parser ["-F", "true"] == Ok True

# bool dashed short argument with value is determined false
expect
    parser = bool { long: "foo", short: Some "F" }

    parseHelp parser ["-F", "false"] == Ok False

# bool dashed short argument with value is determined wrong type
expect
    parser = bool { long: "foo", short: Some "F" }

    parseHelp parser ["-F", "not-a-bool"] == Err (WrongType { arg: "foo", expected: Bool })

# string dashed long argument without value is missing
expect
    parser = str { long: "foo" }

    parseHelp parser ["--foo"] == Err (MissingRequiredArg "foo")

# string dashed long argument with value is determined
expect
    parser = str { long: "foo" }

    parseHelp parser ["--foo", "itsme"] == Ok "itsme"

# string dashed short argument without value is missing
expect
    parser = str { long: "foo", short: Some "F" }

    parseHelp parser ["-F"] == Err (MissingRequiredArg "foo")

# string dashed short argument with value is determined
expect
    parser = str { long: "foo", short: Some "F" }

    parseHelp parser ["-F", "itsme"] == Ok "itsme"

# two string parsers complete cases
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo) bar: \(bar)")
        |> withParser (str { long: "foo" })
        |> withParser (str { long: "bar" })

    cases = [
        ["--foo", "true", "--bar", "baz"],
        ["--bar", "baz", "--foo", "true"],
        ["--foo", "true", "--bar", "baz", "--other", "something"],
    ]

    List.all cases \args -> parseHelp parser args == Ok "foo: true bar: baz"

# string and bool parsers build help
expect
    parser =
        succeed (\foo -> \bar -> \_bool -> "foo: \(foo) bar: \(bar)")
        |> withParser (str { long: "foo", help: Some "the foo flag" })
        |> withParser (str { long: "bar", short: Some "B" })
        |> withParser (bool { long: "bool" })

    toHelp parser
    == Config [
        { long: "foo", short: NotProvided, help: Some "the foo flag", type: Str },
        { long: "bar", short: Some "B", help: NotProvided, type: Str },
        { long: "bool", short: NotProvided, help: NotProvided, type: Bool },
    ]

# format argument is missing
expect
    parser = bool { long: "foo" }

    when parseHelp parser ["foo"] is
        Ok _ -> False
        Err e ->
            err = formatError e
            err == "Argument `--foo` is required but was not provided!"

# format argument has wrong type
expect
    parser = bool { long: "foo" }

    when parseHelp parser ["--foo", "12"] is
        Ok _ -> False
        Err e ->
            err = formatError e
            err == "The argument `--foo` expects a value of type bool!"

# format help menu with only args
expect
    parser =
        succeed (\_foo -> \_bar -> \_baz -> \_bool -> "")
        |> withParser (str { long: "foo", help: Some "the foo flag" })
        |> withParser (str { long: "bar", short: Some "B" })
        |> withParser (str { long: "baz", short: Some "z", help: Some "the baz flag" })
        |> withParser (bool { long: "bool" })
        |> named "test"

    formatHelp parser ==
        """
        test

        OPTIONS:
            --foo    the foo flag  (string)
            --bar, -B  (string)
            --baz, -z    the baz flag  (string)
            --bool  (bool)
        """

# format help menu with subcommands
expect
    parser =
        choice [
            subCommand
                "login"
                (succeed (\user -> \pw -> "\(user)\(pw)")
                 |> withParser (str { long: "user" })
                 |> withParser (str { long: "pw" })),
            subCommand
                "publish"
                (succeed (\file -> \url -> "\(file)\(url)")
                 |> withParser (str { long: "file" })
                 |> withParser (str { long: "url" })),
        ]
        |> named "test"

    formatHelp parser ==
        """
        test

        COMMANDS:
            login
                --user  (string)
                --pw  (string)

            publish
                --file  (string)
                --url  (string)
        """

# subcommand parser
expect
    parser =
        choice [
            subCommand
                "login"
                (succeed (\user -> \pw -> "logging in \(user) with \(pw)")
                 |> withParser (str { long: "user" })
                 |> withParser (str { long: "pw" })),
            subCommand
                "publish"
                (succeed (\file -> \url -> "\(file)\(url)")
                 |> withParser (str { long: "file" })
                 |> withParser (str { long: "url" })),
        ]
        |> named "test"

    when parse parser ["test", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> False

# subcommand of subcommand parser
expect
    parser =
        choice [
            subCommand
                "auth"
                (choice [
                    subCommand
                        "login"
                        (succeed (\user -> \pw -> "logging in \(user) with \(pw)")
                         |> withParser (str { long: "user" })
                         |> withParser (str { long: "pw" })),
                ])
        ]
        |> named "test"

    when parse parser ["test", "auth", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> False

# subcommand not provided
expect
    parser =
        choice [ subCommand "auth" (succeed ""), subCommand "publish" (succeed "") ]

    when parseHelp parser [] is
        Ok _ -> True
        Err e ->
            err = formatError e
            err ==
                """
                A subcommand was expected, but not found!
                The available subcommands are:
                \t"auth", "publish"
                """

# subcommand doesn't match choices
expect
    parser =
        choice [ subCommand "auth" (succeed ""), subCommand "publish" (succeed "") ]

    when parseHelp parser ["logs"] is
        Ok _ -> True
        Err e ->
            err = formatError e
            err ==
                """
                The "logs" subcommand was found, but it's not expected in this context! 
                The available subcommands are:
                \t"auth", "publish"
                """
