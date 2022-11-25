interface Arg
    exposes [
        Parser,
        NamedParser,
        parse,
        toHelp,
        parseFormatted,
        succeed,
        boolOption,
        strOption,
        i64Option,
        str,
        subCommand,
        choice,
        withParser,
        program,
        list,
    ]
    imports [Effect, InternalTask, Task.{ Task }]

## Gives a list of the program's command-line arguments.
list : Task (List Str) *
list =
    Effect.args
    |> Effect.map Ok
    |> InternalTask.fromEffect

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
    Option OptionConfig (MarkedArgs -> Result { newlyTaken : Taken, val : a } (ParseError [])),
    Positional PositionalConfig (MarkedArgs -> Result { newlyTaken : Taken, val : a } (ParseError [])),
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

## Indices in an arguments list that have already been parsed.
Taken : Set Nat

## A representation of parsed and unparsed arguments in a constant list of
## command-line arguments.
## Used only internally, for efficient representation of parsed and unparsed
## arguments.
MarkedArgs : { args : List Str, taken : Taken }

## Enumerates errors that can occur during parsing a list of command line arguments.
ParseError a : [
    ## The program name was not found as the first argument to be parsed.
    ProgramNameNotProvided Str,
    ## A positional argument (inherently required) was not found.
    MissingPositional Str,
    ## An option argument is required, but it was not found.
    MissingRequiredOption Str,
    ## An argument was found, but it didn't have the expected [OptionType].
    WrongOptionType
        {
            arg : Str,
            expected : OptionType,
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

## Expected type of an option, in an argument list being parsed.
## Describes how a string option should be interpreted as a certain type.
OptionType : [
    Str,
    Bool,
    I64,
]

## Help metadata extracted from a [Parser].
Help : [
    SubCommands (List { name : Str, help : Help }),
    Config (List Config),
]

OptionConfig : {
    long : Str,
    short : Str,
    help : Str,
    type : OptionType,
}

PositionalConfig : {
    name : Str,
    help : Str,
}

Config : [Option OptionConfig, Positional PositionalConfig]

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

toHelpHelper : Parser *, List Config -> Help
toHelpHelper = \@Parser parser, configs ->
    when parser is
        Succeed _ -> Config configs
        Lazy _ -> Config configs
        WithConfig innerParser config ->
            toHelpHelper innerParser (List.append configs config)

        Option config _ ->
            List.append configs (Option config)
            |> Config

        SubCommand commands ->
            List.map
                commands
                (\{ name, parser: innerParser } -> { name, help: toHelpHelper innerParser [] })
            |> SubCommands

        Positional config _ ->
            List.append configs (Positional config)
            |> Config

findOneArg : Str, Str, MarkedArgs -> Result { val : Str, newlyTaken : Taken } [NotFound]
findOneArg = \long, short, { args, taken } ->
    argMatches = \{ index, found: _ }, arg ->
        if Set.contains taken index || Set.contains taken (index + 1) then
            Continue { index: index + 1, found: Bool.false }
        else if arg == "--\(long)" then
            Break { index, found: Bool.true }
        else if Bool.not (Str.isEmpty short) && arg == "-\(short)" then
            Break { index, found: Bool.true }
        else
            Continue { index: index + 1, found: Bool.false }

    # TODO allow = as well, etc.
    { index: argIndex, found } = List.walkUntil args { index: 0, found: Bool.false } argMatches

    if !found then
        Err NotFound
    else
        # Return the next argument after the given one
        List.get args (argIndex + 1)
        |> Result.mapErr (\_ -> NotFound)
        |> Result.map
            (\val ->
                newUsed = Set.fromList [argIndex, argIndex + 1]

                { val, newlyTaken: newUsed })

updateTaken : MarkedArgs, Taken -> MarkedArgs
updateTaken = \{ args, taken }, taken2 -> { args, taken: Set.union taken taken2 }

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

                    Option config run ->
                        Option config \args ->
                            run args
                            |> Result.map (\{ val, newlyTaken } -> { val: fn val, newlyTaken })

                    Positional config run ->
                        Positional config \args ->
                            run args
                            |> Result.map (\{ val, newlyTaken } -> { val: fn val, newlyTaken })

                    SubCommand cmds ->
                        mapSubParser = \{ name, parser: parser2 } ->
                            { name, parser: andMap parser2 (@Parser mapper) }

                        List.map cmds mapSubParser
                        |> SubCommand

            Option config run ->
                when parser is
                    Succeed a ->
                        Option config \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } -> Ok { val: fn a, newlyTaken }
                                Err err -> Err err

                    Lazy thunk ->
                        Option config \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } -> Ok { val: fn (thunk {}), newlyTaken }
                                Err err -> Err err

                    WithConfig parser2 config2 ->
                        parser2
                        |> andMap (@Parser mapper)
                        |> WithConfig config2

                    Option config2 run2 ->
                        # Parse first the one and then the other.
                        combinedParser = Option config2 \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } ->
                                    run2 (updateTaken args newlyTaken)
                                    |> Result.map (\{ val, newlyTaken: newlyTaken2 } -> { val: fn val, newlyTaken: Set.union newlyTaken newlyTaken2 })

                                Err err -> Err err

                        # Store the extra config.
                        @Parser combinedParser
                        |> WithConfig (Option config)

                    Positional config2 run2 ->
                        combinedParser = Positional config2 \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } ->
                                    run2 (updateTaken args newlyTaken)
                                    |> Result.map (\{ val, newlyTaken: newlyTaken2 } -> { val: fn val, newlyTaken: Set.union newlyTaken newlyTaken2 })

                                Err err -> Err err

                        # Store the extra config.
                        @Parser combinedParser
                        |> WithConfig (Option config)

                    SubCommand cmds ->
                        # For each subcommand, first run the subcommand, then
                        # push the result through the arg parser.
                        mapSubParser = \{ name, parser: parser2 } ->
                            { name, parser: andMap parser2 (@Parser mapper) }

                        List.map cmds mapSubParser
                        |> SubCommand

            Positional config run ->
                when parser is
                    Succeed a ->
                        Positional config \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } -> Ok { val: fn a, newlyTaken }
                                Err err -> Err err

                    Lazy thunk ->
                        Positional config \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } -> Ok { val: fn (thunk {}), newlyTaken }
                                Err err -> Err err

                    WithConfig parser2 config2 ->
                        parser2
                        |> andMap (@Parser mapper)
                        |> WithConfig config2

                    Option config2 run2 ->
                        # Parse first the one and then the other.
                        combinedParser = Option config2 \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } ->
                                    run2 (updateTaken args newlyTaken)
                                    |> Result.map (\{ val, newlyTaken: newlyTaken2 } -> { val: fn val, newlyTaken: Set.union newlyTaken newlyTaken2 })

                                Err err -> Err err

                        # Store the extra config.
                        @Parser combinedParser
                        |> WithConfig (Positional config)

                    Positional config2 run2 ->
                        combinedParser = Positional config2 \args ->
                            when run args is
                                Ok { val: fn, newlyTaken } ->
                                    run2 (updateTaken args newlyTaken)
                                    |> Result.map (\{ val, newlyTaken: newlyTaken2 } -> { val: fn val, newlyTaken: Set.union newlyTaken newlyTaken2 })

                                Err err -> Err err

                        # Store the extra config.
                        @Parser combinedParser
                        |> WithConfig (Positional config)

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

                    Option config run ->
                        Option config \args ->
                            run args
                            |> Result.map (\{ val, newlyTaken } -> { val: fn val, newlyTaken })

                    Positional config run ->
                        Positional config \args ->
                            run args
                            |> Result.map (\{ val, newlyTaken } -> { val: fn val, newlyTaken })

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
    # By convention the first string in the argument list is the program name.
    if
        List.isEmpty args
    then
        Err (ProgramNameNotProvided parser.name)
    else
        markedArgs = { args, taken: Set.single 0 }

        parseHelp parser.parser markedArgs

parseHelp : Parser a, MarkedArgs -> Result a (ParseError [])
parseHelp = \@Parser parser, args ->
    when parser is
        Succeed val -> Ok val
        Option _ run ->
            run args
            |> Result.map .val

        Positional _ run ->
            run args
            |> Result.map .val

        SubCommand cmds ->
            when nextUnmarked args is
                Ok { index, val: cmd } ->
                    argsRest = { args & taken: Set.insert args.taken index }
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

nextUnmarked : MarkedArgs -> Result { index : Nat, val : Str } [OutOfBounds]
nextUnmarked = \marked ->
    help = \index ->
        if Set.contains marked.taken index then
            help (index + 1)
        else
            List.get marked.args index
            |> Result.map \val -> { index, val }

    help 0

## Creates a parser for a boolean option argument.
## Options of value "true" and "false" will be parsed as [Bool.true] and [Bool.false], respectively.
## All other values will result in a `WrongOptionType` error.
boolOption : _ -> Parser Bool # TODO: panics if parameter annotation given
boolOption = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err (MissingRequiredOption long)
            Ok { val, newlyTaken } ->
                when val is
                    "true" -> Ok { val: Bool.true, newlyTaken }
                    "false" -> Ok { val: Bool.false, newlyTaken }
                    _ -> Err (WrongOptionType { arg: long, expected: Bool })

    @Parser (Option { long, short, help, type: Bool } fn)

## Creates a parser for a string option argument.
strOption : _ -> Parser Str # TODO: panics if parameter annotation given
strOption = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err (MissingRequiredOption long)
            Ok { val, newlyTaken } -> Ok { val, newlyTaken }

    @Parser (Option { long, short, help, type: Str } fn)

## Creates a parser for a 64-bit signed integer ([I64]) option argument.
i64Option : _ -> Parser I64 # TODO: panics if parameter annotation given
i64Option = \{ long, short ? "", help ? "" } ->
    fn = \args ->
        when findOneArg long short args is
            Err NotFound -> Err (MissingRequiredOption long)
            Ok { val, newlyTaken } ->
                Str.toI64 val
                |> Result.mapErr (\_ -> WrongOptionType { arg: long, expected: I64 })
                |> Result.map (\v -> { val: v, newlyTaken })

    @Parser (Option { long, short, help, type: I64 } fn)

## Parses a single positional argument as a string.
str : _ -> Parser Str
str = \{ name, help ? "" } ->
    fn = \args ->
        nextUnmarked args
        |> Result.mapErr (\OutOfBounds -> MissingPositional name)
        |> Result.map (\{ val, index } -> { val, newlyTaken: Set.insert args.taken index })

    @Parser (Positional { name, help } fn)

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

filterMap : List a, (a -> [Some b, None]) -> List b
filterMap = \lst, transform ->
    List.walk lst [] \all, elem ->
        when transform elem is
            Some v -> List.append all v
            None -> all

# formatHelp : NamedParser a -> Str
formatHelp = \@NamedParser { name, help, parser } ->
    fmtHelp =
        mapNonEmptyStr help \helpStr -> "\n\(helpStr)"

    cmdHelp = toHelp parser

    fmtCmdHelp = formatHelpHelp 0 cmdHelp

    """
    \(name)\(fmtHelp)
    \(fmtCmdHelp)
    """

# formatHelpHelp : Nat, Help -> Str
formatHelpHelp = \n, cmdHelp ->
    indented = indent n

    when cmdHelp is
        SubCommands cmds ->
            fmtCmdHelp =
                Str.joinWith
                    (List.map cmds \subCmd -> formatSubCommand (n + indentLevel) subCmd)
                    "\n\n"

            """
            
            \(indented)COMMANDS:
            \(fmtCmdHelp)
            """

        Config configs ->
            optionConfigs =
                filterMap
                    configs
                    (\config ->
                        when config is
                            Option c -> Some c
                            _ -> None)

            positionalConfigs =
                filterMap
                    configs
                    (\config ->
                        when config is
                            Positional c -> Some c
                            _ -> None)

            fmtOptionsHelp =
                if List.isEmpty optionConfigs then
                    ""
                else
                    helpStr =
                        optionConfigs
                        |> List.map (\c -> formatOptionConfig (n + indentLevel) c)
                        |> Str.joinWith "\n"

                    """
                    
                    \(indented)OPTIONS:
                    \(helpStr)
                    """

            fmtPositionalsHelp =
                if List.isEmpty positionalConfigs then
                    ""
                else
                    helpStr =
                        positionalConfigs
                        |> List.map (\c -> formatPositionalConfig (n + indentLevel) c)
                        |> Str.joinWith "\n"

                    """
                    
                    \(indented)ARGS:
                    \(helpStr)
                    """

            Str.concat fmtPositionalsHelp fmtOptionsHelp

formatSubCommand = \n, { name, help } ->
    indented = indent n

    fmtHelp = formatHelpHelp (n + indentLevel) help

    "\(indented)\(name)\(fmtHelp)"

formatOptionConfig : Nat, OptionConfig -> Str
formatOptionConfig = \n, { long, short, help, type } ->
    indented = indent n

    formattedShort =
        mapNonEmptyStr short \s -> ", -\(s)"

    formattedType = formatOptionType type

    formattedHelp =
        mapNonEmptyStr help \h -> "    \(h)"

    "\(indented)--\(long)\(formattedShort)\(formattedHelp)  (\(formattedType))"

formatPositionalConfig : Nat, PositionalConfig -> Str
formatPositionalConfig = \n, { name, help } ->
    indented = indent n

    formattedHelp =
        mapNonEmptyStr help \h -> "    \(h)"

    "\(indented)\(name)\(formattedHelp)"

formatOptionType : OptionType -> Str
formatOptionType = \type ->
    when type is
        Bool -> "boolean"
        Str -> "string"
        I64 -> "integer, 64-bit signed"

quote = \s -> "\"\(s)\""

formatError : ParseError [] -> Str
formatError = \err ->
    when err is
        ProgramNameNotProvided programName ->
            "The program name \"\(programName)\" was not provided as a first argument!"

        MissingPositional arg ->
            "The argument `\(arg)` is required but was not provided!"

        MissingRequiredOption arg ->
            "The option `--\(arg)` is required but was not provided!"

        WrongOptionType { arg, expected } ->
            formattedType = formatOptionType expected

            "The option `--\(arg)` expects a value of type \(formattedType)!"

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
## example, to combine the results of two [strOption] arguments into a record,
## you could use
##
## ```
## succeed (\host -> \port -> { host, port })
## |> withParser (strOption { long: "host" })
## |> withParser (strOption { long: "port" })
## ```
withParser = \arg1, arg2 -> andMap arg2 arg1

mark = \args -> { args, taken: Set.empty }

# boolean undashed long optional is missing
expect
    parser = boolOption { long: "foo" }

    parseHelp parser (mark ["foo"]) == Err (MissingRequiredOption "foo")

# boolean dashed long optional without value is missing
expect
    parser = boolOption { long: "foo" }

    parseHelp parser (mark ["--foo"]) == Err (MissingRequiredOption "foo")

# boolean dashed long optional with value is determined true
expect
    parser = boolOption { long: "foo" }

    parseHelp parser (mark ["--foo", "true"]) == Ok Bool.true

# boolean dashed long optional with value is determined false
expect
    parser = boolOption { long: "foo" }

    parseHelp parser (mark ["--foo", "false"]) == Ok Bool.false

# boolean dashed long optional with value is determined wrong type
expect
    parser = boolOption { long: "foo" }

    parseHelp parser (mark ["--foo", "not-a-boolean"]) == Err (WrongOptionType { arg: "foo", expected: Bool })

# boolean dashed short optional with value is determined true
expect
    parser = boolOption { long: "foo", short: "F" }

    parseHelp parser (mark ["-F", "true"]) == Ok Bool.true

# boolean dashed short optional with value is determined false
expect
    parser = boolOption { long: "foo", short: "F" }

    parseHelp parser (mark ["-F", "false"]) == Ok Bool.false

# boolean dashed short optional with value is determined wrong type
expect
    parser = boolOption { long: "foo", short: "F" }

    parseHelp parser (mark ["-F", "not-a-boolean"]) == Err (WrongOptionType { arg: "foo", expected: Bool })

# string dashed long option without value is missing
expect
    parser = strOption { long: "foo" }

    parseHelp parser (mark ["--foo"]) == Err (MissingRequiredOption "foo")

# string dashed long option with value is determined
expect
    parser = strOption { long: "foo" }

    parseHelp parser (mark ["--foo", "itsme"]) == Ok "itsme"

# string dashed short option without value is missing
expect
    parser = strOption { long: "foo", short: "F" }

    parseHelp parser (mark ["-F"]) == Err (MissingRequiredOption "foo")

# string dashed short option with value is determined
expect
    parser = strOption { long: "foo", short: "F" }

    parseHelp parser (mark ["-F", "itsme"]) == Ok "itsme"

# i64 dashed long option without value is missing
expect
    parser = i64Option { long: "foo" }

    parseHelp parser (mark ["--foo"]) == Err (MissingRequiredOption "foo")

# i64 dashed long option with value is determined positive
expect
    parser = i64Option { long: "foo" }

    parseHelp parser (mark ["--foo", "1234"]) == Ok 1234

# i64 dashed long option with value is determined negative
expect
    parser = i64Option { long: "foo" }

    parseHelp parser (mark ["--foo", "-1234"]) == Ok -1234

# i64 dashed short option without value is missing
expect
    parser = i64Option { long: "foo", short: "F" }

    parseHelp parser (mark ["-F"]) == Err (MissingRequiredOption "foo")

# i64 dashed short option with value is determined
expect
    parser = i64Option { long: "foo", short: "F" }

    parseHelp parser (mark ["-F", "1234"]) == Ok 1234

# two string parsers complete cases
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo) bar: \(bar)")
        |> withParser (strOption { long: "foo" })
        |> withParser (strOption { long: "bar" })

    cases = [
        ["--foo", "true", "--bar", "baz"],
        ["--bar", "baz", "--foo", "true"],
        ["--foo", "true", "--bar", "baz", "--other", "something"],
    ]

    List.all cases \args -> parseHelp parser (mark args) == Ok "foo: true bar: baz"

# one argument is missing out of multiple
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo) bar: \(bar)")
        |> withParser (strOption { long: "foo" })
        |> withParser (strOption { long: "bar" })

    List.all
        [
            parseHelp parser (mark ["--foo", "zaz"]) == Err (MissingRequiredOption "bar"),
            parseHelp parser (mark ["--bar", "zaz"]) == Err (MissingRequiredOption "foo"),
        ]
        (\b -> b)

# string and boolean parsers build help
expect
    parser =
        succeed (\foo -> \bar -> \_bool -> "foo: \(foo) bar: \(bar)")
        |> withParser (strOption { long: "foo", help: "the foo option" })
        |> withParser (strOption { long: "bar", short: "B" })
        |> withParser (boolOption { long: "boolean" })

    toHelp parser
    == Config [
        Option { long: "foo", short: "", help: "the foo option", type: Str },
        Option { long: "bar", short: "B", help: "", type: Str },
        Option { long: "boolean", short: "", help: "", type: Bool },
    ]

# format option is missing
expect
    parser = boolOption { long: "foo" }

    when parseHelp parser (mark ["foo"]) is
        Ok _ -> Bool.false
        Err e ->
            err = formatError e

            err == "The option `--foo` is required but was not provided!"

# format option has wrong type
expect
    parser = boolOption { long: "foo" }

    when parseHelp parser (mark ["--foo", "12"]) is
        Ok _ -> Bool.false
        Err e ->
            err = formatError e

            err == "The option `--foo` expects a value of type boolean!"

# format help menu with only options
expect
    parser =
        succeed (\_foo -> \_bar -> \_baz -> \_bool -> "")
        |> withParser (strOption { long: "foo", help: "the foo option" })
        |> withParser (strOption { long: "bar", short: "B" })
        |> withParser (strOption { long: "baz", short: "z", help: "the baz option" })
        |> withParser (boolOption { long: "boolean" })
        |> program { name: "test" }

    formatHelp parser
    ==
    """
    test
    
    OPTIONS:
        --foo    the foo option  (string)
        --bar, -B  (string)
        --baz, -z    the baz option  (string)
        --boolean  (boolean)
    """

# format help menu with subcommands
expect
    parser =
        choice [
            succeed (\user -> \pw -> "\(user)\(pw)")
            |> withParser (strOption { long: "user" })
            |> withParser (strOption { long: "pw" })
            |> subCommand "login",
            succeed (\file -> \url -> "\(file)\(url)")
            |> withParser (strOption { long: "file" })
            |> withParser (strOption { long: "url" })
            |> subCommand "publish",
        ]
        |> program { name: "test" }

    formatHelp parser
    ==
    """
    test
    
    COMMANDS:
        login
            OPTIONS:
                --user  (string)
                --pw  (string)
    
        publish
            OPTIONS:
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
            |> withParser (strOption { long: "user" })
            |> withParser (strOption { long: "pw" })
            |> subCommand "login",
            succeed (\file -> \url -> "\(file)\(url)")
            |> withParser (strOption { long: "file" })
            |> withParser (strOption { long: "url" })
            |> subCommand "publish",
        ]
        |> program { name: "test" }

    when parse parser ["test", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> Bool.false

# subcommand of subcommand parser
expect
    parser =
        choice [
            choice [
                succeed (\user -> \pw -> "logging in \(user) with \(pw)")
                |> withParser (strOption { long: "user" })
                |> withParser (strOption { long: "pw" })
                |> subCommand "login",
            ]
            |> subCommand "auth",
        ]
        |> program { name: "test" }

    when parse parser ["test", "auth", "login", "--pw", "123", "--user", "abc"] is
        Ok result -> result == "logging in abc with 123"
        Err _ -> Bool.false

# subcommand not provided
expect
    parser =
        choice [subCommand (succeed "") "auth", subCommand (succeed "") "publish"]

    when parseHelp parser (mark []) is
        Ok _ -> Bool.true
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

    when parseHelp parser (mark ["logs"]) is
        Ok _ -> Bool.true
        Err e ->
            err = formatError e

            err
            ==
            """
            The "logs" subcommand was found, but it's not expected in this context!
            The available subcommands are:
            \t"auth", "publish"
            """

# parse positional
expect
    parser = str { name: "foo" }

    parseHelp parser (mark ["myArg"]) == Ok "myArg"

# parse positional with option
expect
    parser =
        succeed (\foo -> \bar -> "foo: \(foo), bar: \(bar)")
        |> withParser (strOption { long: "foo" })
        |> withParser (str { name: "bar" })

    cases = [
        ["--foo", "true", "baz"],
        ["baz", "--foo", "true"],
    ]

    List.all cases \args -> parseHelp parser (mark args) == Ok "foo: true, bar: baz"

# parse positional with subcommand
expect
    parser = choice [
        str { name: "bar" }
        |> subCommand "hello",
    ]

    parseHelp parser (mark ["hello", "foo"]) == Ok "foo"

# missing positional
expect
    parser = str { name: "bar" }

    parseHelp parser (mark []) == Err (MissingPositional "bar")
