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
        i64,
        subCommand,
        choice,
        withParser,
        program,
    ]
    imports []

## A parser for a command-line application.
## A [NamedParser] is usually built from a [Parser] using [program].
NamedParser a := {
    name : Str,
    help : Str,
    parser : Parser a,
}

## Describes how to parse a slice of command-line arguments.
## [Parser]s can be composed in various ways, including via [withParser] and
## [subCommand].
## Once you have a [Parser] that describes your application's entire parsing
## needs, consider transforming it into a [NamedParser].
Parser a := [
    Succeed a,
    Arg Config (List Str -> Result a [NotFound, WrongType]),
    # TODO: hiding the record behind an alias currently causes a panic
    SubCommand
        (List {
            name : Str,
            parser : Parser a,
        }),

    # Constructed during transformations of the above variants
    WithConfig (Parser a) Config,
    Lazy ({} -> a),
]

## Enumerates errors that can occur during parsing a list of command line arguments.
ParseError a : [
    ## The program name was not found as the first argument to be parsed.
    ProgramNameNotProvided Str,
    ## An argument is required, but it was not found.
    MissingRequiredArg Str,
    ## An argument was found, but it didn't have the expected [Type].
    WrongType
        {
            arg : Str,
            expected : Type,
        },
    ## A subcommand is required, but it was not found.
    SubCommandNotFound
        {
            choices : List Str,
        },
    ## A subcommand was found, but it was not the expected one.
    IncorrectSubCommand
        {
            found : Str,
            choices : List Str,
        },
]a

## Expected type of an argument, in an argument list being parsed.
## Describes how a string argument should be interpreted as a certain type.
Type : [
    Str,
    Bool,
    I64,
]

## Help metadata extracted from a [Parser].
Help : [
    SubCommands (List { name : Str, help : Help }),
    Config (List Config),
]

Config : {
    long : Str,
    short : Str,
    help : Str,
    type : Type,
}

## Generates help metadata from a [Parser].
##
## This is useful if you would like to use this metadata to generate your own
## human-readable help or hint menus.
##
## A default help menu can be generated with [formatHelp].
toHelp : Parser * -> Help
toHelp = \parser ->
    toHelpHelper parser []

## A parser that immediately succeeds with its given input.
succeed : a -> Parser a
succeed = \val -> @Parser (Succeed val)

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
                (\{ name, parser: innerParser } -> { name, help: toHelpHelper innerParser [] })
            |> SubCommands

findOneArg : Str, Str, List Str -> Result Str [NotFound]*
findOneArg = \long, short, args ->
    argMatches = \arg ->
        if arg == "--\(long)" then
            True
        else
            Bool.not (Str.isEmpty short) && arg == "-\(short)"

    # TODO allow = as well, etc.
    result = List.findFirstIndex args argMatches

    when result is
        Ok index ->
            # Return the next arg after the given one
            List.get args (index + 1)
            |> Result.mapErr \_ -> NotFound

        Err NotFound -> Err NotFound

# andMap : Parser a, Parser (a -> b) -> Parser b
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
                        mapSubParser = \{ name, parser: parser2 } ->
                            { name, parser: andMap parser2 (@Parser mapper) }

                        List.map cmds mapSubParser
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
                        mapSubParser = \{ name, parser: parser2 } ->
                            { name, parser: andMap parser2 (@Parser mapper) }

                        List.map cmds mapSubParser
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
                        mapSubParser = \{ name, parser: parser2 } ->
                            { name, parser: andMap parser2 (@Parser mapper) }

                        List.map cmds mapSubParser
                        |> SubCommand

            WithConfig mapper2 config ->
                @Parser parser
                |> andMap mapper2
                |> WithConfig config

            SubCommand cmds ->
                mapSubParser = \{ name, parser: mapper2 } ->
                    { name, parser: andMap (@Parser parser) mapper2 }

                List.map cmds mapSubParser
                |> SubCommand

    @Parser unwrapped

## Marks a [Parser] as the entry point for parsing a command-line application,
## taking the program name and optionally a high-level help message for the
## application.
##
## The produced [NamedParser] can be used to parse arguments via [parse] or
## [parseFormatted].
program = \parser, { name, help ? "" } ->
    @NamedParser { name, help, parser }

## Parses a list of command-line arguments with the given parser. The list of
## arguments is expected to contain the name of the program in the first
## position.
##
## If the arguments do not conform with what is expected by the parser, the
## first error seen will be returned.
# TODO panics in alias analysis when this annotation is included
# parse : NamedParser a, List Str -> Result a (ParseError*)
parse = \@NamedParser parser, args ->
    # By convention the first string in the arg list is the program name.
    if
        List.isEmpty args
    then
        Err (ProgramNameNotProvided parser.name)
    else
        parseHelp parser.parser (List.split args 1).others

parseHelp : Parser a, List Str -> Result a (ParseError *)
parseHelp = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Arg { long, type } run ->
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
                            \st, { name, parser: subParser } ->
                                if
                                    cmd == name
                                then
                                    Break (Ok (parseHelp subParser argsRest))
                                else
                                    Continue st

                    when state is
                        Ok result -> result
                        Err {} -> Err (IncorrectSubCommand { found: cmd, choices: List.map cmds .name })

                Err OutOfBounds -> Err (SubCommandNotFound { choices: List.map cmds .name })

        Lazy thunk -> Ok (thunk {})
        WithConfig parser2 _config ->
            parseHelp parser2 args

## Creates a parser for a boolean flag argument.
## Flags of value "true" and "false" will be parsed as [True] and [False], respectively.
## All other values will result in a `WrongType` error.
bool : _ -> Parser Bool # TODO: panics if parameter annotation given
bool = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok "true" -> Ok True
            Ok "false" -> Ok False
            Ok _ -> Err WrongType

    @Parser (Arg { long, short, help, type: Bool } fn)

## Creates a parser for a string flag argument.
str : _ -> Parser Str # TODO: panics if parameter annotation given
str = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg -> Ok foundArg

    @Parser (Arg { long, short, help, type: Str } fn)

## Creates a parser for a 64-bit signed integer ([I64]).
i64 : _ -> Parser I64 # TODO: panics if parameter annotation given
i64 = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err NotFound
            Ok foundArg ->
                Str.toI64 foundArg
                |> Result.mapErr \_ -> WrongType

    @Parser (Arg { long, short, help, type: I64 } fn)

## Wraps a given parser as a subcommand parser.
##
## When parsing arguments, the subcommand name will be expected to be parsed
## first, and then the wrapped parser will be applied to the rest of the
## arguments.
##
## To support multiple subcommands, use [choice].
subCommand : Parser a, Str -> { name : Str, parser : Parser a }
subCommand = \parser, name -> { name, parser }

## Creates a parser that matches over a list of subcommands.
##
## The given list of subcommands is expected to be non-empty, and unique in the
## subcommand name. These invariants are not enforced today, but may be in the
## future.
##
## During argument parsing, the list of subcommands will be tried in-order. Due
## to the described invariant, at most one given subcommand will match any
## argument list.
choice : List { name : Str, parser : Parser a } -> Parser a
choice = \subCommands -> @Parser (SubCommand subCommands)

## Like [parse], runs a parser to completion on a list of arguments.
##
## If the parser fails, a formatted error and help message is returned.
# TODO: mono panics in the args example if the type annotation is included
# parseFormatted : NamedParser a, List Str -> Result a Str
parseFormatted = \@NamedParser parser, args ->
    Result.mapErr
        (parse (@NamedParser parser) args)
        \e ->
            Str.concat (Str.concat (formatHelp (@NamedParser parser)) "\n\n") (formatError e)

indent : Nat -> Str
indent = \n -> Str.repeat " " n

indentLevel : Nat
indentLevel = 4

mapNonEmptyStr = \s, f -> if Str.isEmpty s then s else f s

# formatHelp : NamedParser a -> Str
formatHelp = \@NamedParser { name, help, parser } ->
    fmtHelp =
        mapNonEmptyStr help \helpStr -> "\n\(helpStr)"

    cmdHelp = toHelp parser

    fmtCmdHeading =
        when cmdHelp is
            SubCommands _ -> "COMMANDS:"
            Config _ -> "OPTIONS:"

    fmtCmdHelp = formatCmdHelp indentLevel cmdHelp

    """
    \(name)\(fmtHelp)
    
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

formatSubCommand = \n, { name, help } ->
    indented = indent n

    fmtHelp = formatCmdHelp (n + indentLevel) help

    "\(indented)\(name)\n\(fmtHelp)"

formatConfig : Nat, Config -> Str
formatConfig = \n, { long, short, help, type } ->
    indented = indent n

    formattedShort =
        mapNonEmptyStr short \s -> ", -\(s)"

    formattedType = formatType type

    formattedHelp =
        mapNonEmptyStr help \h -> "    \(h)"

    "\(indented)--\(long)\(formattedShort)\(formattedHelp)  (\(formattedType))"

formatType : Type -> Str
formatType = \type ->
    when type is
        Bool -> "bool"
        Str -> "string"
        I64 -> "i64"

quote = \s -> "\"\(s)\""

formatError : ParseError [] -> Str
formatError = \err ->
    when err is
        ProgramNameNotProvided programName ->
            "The program name \"\(programName)\" was not probided as a first argument!"

        MissingRequiredArg arg ->
            "Argument `--\(arg)` is required but was not provided!"

        WrongType { arg, expected } ->
            formattedType = formatType expected

            "The argument `--\(arg)` expects a value of type \(formattedType)!"

        SubCommandNotFound { choices } ->
            fmtChoices =
                List.map choices quote
                |> Str.joinWith ", "

            """
            A subcommand was expected, but not found!
            The available subcommands are:
            \t\(fmtChoices)
            """

        IncorrectSubCommand { found, choices } ->
            fmtFound = quote found

            fmtChoices =
                List.map choices quote
                |> Str.joinWith ", "

            """
            The \(fmtFound) subcommand was found, but it's not expected in this context! 
            The available subcommands are:
            \t\(fmtChoices)
            """

## Applies one parser over another, mapping parser.
##
## `withParser mapper parser` produces a parser that will parse an argument list
## with `parser` first, then parse the remaining list with `mapper`, and feed
## the result of `parser` to `mapper`.
##
## This provides a way to chain the results of multiple parsers together. For
## example, to combine the results of two [str] arguments into a record, you
## could use
##
## ```
## succeed (\host -> \port -> { host, port })
## |> withParser (str { long: "host" })
## |> withParser (str { long: "port" })
## ```
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
    parser = bool { long: "foo", short: "F" }

    parseHelp parser ["-F", "true"] == Ok True

# bool dashed short argument with value is determined false
expect
    parser = bool { long: "foo", short: "F" }

    parseHelp parser ["-F", "false"] == Ok False

# bool dashed short argument with value is determined wrong type
expect
    parser = bool { long: "foo", short: "F" }

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
    parser = str { long: "foo", short: "F" }

    parseHelp parser ["-F"] == Err (MissingRequiredArg "foo")

# string dashed short argument with value is determined
expect
    parser = str { long: "foo", short: "F" }

    parseHelp parser ["-F", "itsme"] == Ok "itsme"

# i64 dashed long argument without value is missing
expect
    parser = i64 { long: "foo" }

    parseHelp parser ["--foo"] == Err (MissingRequiredArg "foo")

# i64 dashed long argument with value is determined positive
expect
    parser = i64 { long: "foo" }

    parseHelp parser ["--foo", "1234"] == Ok 1234

# i64 dashed long argument with value is determined negative
expect
    parser = i64 { long: "foo" }

    parseHelp parser ["--foo", "-1234"] == Ok -1234

# i64 dashed short argument without value is missing
expect
    parser = i64 { long: "foo", short: "F" }

    parseHelp parser ["-F"] == Err (MissingRequiredArg "foo")

# i64 dashed short argument with value is determined
expect
    parser = i64 { long: "foo", short: "F" }

    parseHelp parser ["-F", "1234"] == Ok 1234

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
        |> withParser (str { long: "foo", help: "the foo flag" })
        |> withParser (str { long: "bar", short: "B" })
        |> withParser (bool { long: "bool" })

    toHelp parser
    == Config [
        { long: "foo", short: "", help: "the foo flag", type: Str },
        { long: "bar", short: "B", help: "", type: Str },
        { long: "bool", short: "", help: "", type: Bool },
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
        |> withParser (str { long: "foo", help: "the foo flag" })
        |> withParser (str { long: "bar", short: "B" })
        |> withParser (str { long: "baz", short: "z", help: "the baz flag" })
        |> withParser (bool { long: "bool" })
        |> program { name: "test" }

    formatHelp parser
    ==
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
            succeed (\user -> \pw -> "\(user)\(pw)")
            |> withParser (str { long: "user" })
            |> withParser (str { long: "pw" })
            |> subCommand "login",
            succeed (\file -> \url -> "\(file)\(url)")
            |> withParser (str { long: "file" })
            |> withParser (str { long: "url" })
            |> subCommand "publish",
        ]
        |> program { name: "test" }

    formatHelp parser
    ==
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

# format help menu with program help message
expect
    parser =
        choice [subCommand (succeed "") "login"]
        |> program { name: "test", help: "a test cli app" }

    formatHelp parser
    ==
    """
    test
    a test cli app
    
    COMMANDS:
        login
    
    """

# subcommand parser
expect
    parser =
        choice [
            succeed (\user -> \pw -> "logging in \(user) with \(pw)")
            |> withParser (str { long: "user" })
            |> withParser (str { long: "pw" })
            |> subCommand "login",
            succeed (\file -> \url -> "\(file)\(url)")
            |> withParser (str { long: "file" })
            |> withParser (str { long: "url" })
            |> subCommand "publish",
        ]
        |> program { name: "test" }

    when parse parser ["test", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> False

# subcommand of subcommand parser
expect
    parser =
        choice [
            choice [
                succeed (\user -> \pw -> "logging in \(user) with \(pw)")
                |> withParser (str { long: "user" })
                |> withParser (str { long: "pw" })
                |> subCommand "login",
            ]
            |> subCommand "auth",
        ]
        |> program { name: "test" }

    when parse parser ["test", "auth", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> False

# subcommand not provided
expect
    parser =
        choice [subCommand (succeed "") "auth", subCommand (succeed "") "publish"]

    when parseHelp parser [] is
        Ok _ -> True
        Err e ->
            err = formatError e

            err
            ==
            """
            A subcommand was expected, but not found!
            The available subcommands are:
            \t"auth", "publish"
            """

# subcommand doesn't match choices
expect
    parser =
        choice [subCommand (succeed "") "auth", subCommand (succeed "") "publish"]

    when parseHelp parser ["logs"] is
        Ok _ -> True
        Err e ->
            err = formatError e

            err
            ==
            """
            The "logs" subcommand was found, but it's not expected in this context! 
            The available subcommands are:
            \t"auth", "publish"
            """
