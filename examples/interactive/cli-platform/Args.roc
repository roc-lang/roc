

Arg.default : Parser a, a, Config, (Parser (a -> b)) -> Parser b
Arg.required : Parser a, Config, (Parser (a -> b)) -> Parser b
Arg.passthrough : Config, (Parser (List Str -> b)) -> Parser b
Arg.alternatives : List (Str, a) -> Parser a
Arg.subcommands : Str, Parser a, List (Str, Parser a) -> Parser a

Arg.default : Parser a, a, Config -> Parser b
Arg.required : Parser a, Config -> Parser b

parser =
    help = "Build a binary from the given .roc file, but don't run it"

    Arg.subcommands help (Arg.map cmdNone NoSubcommand) [
        ("build", Arg.map cmdBuild Build)
        ("run", Arg.map cmdRun Run)
        ("check", Arg.map cmdCheck Check)
        ("test", Arg.map cmdTest Test)
        ("help", Arg.map cmdHelp Help)
        ("version", Arg.map cmdVersion Version)
    ]

Parser.andThen : Parser a, (a -> Parser b) -> Parser b
Parser.apply : Parser a, Parser (a -> b) -> Parser b

userDecoder : Decoder User
userDecoder =
    Decode.succeed (\id name email -> { id = id, name = name, email = email })
        |> required "id" int
        |> required "name" string
        |> required "email" string

userDecoder : Decoder User
userDecoder =
    Decode.succeed (\id -> \name -> \email -> { id, name, email })
        |> Decode.apply (required "id" int)
        |> Decode.apply (required "name" string)
        |> Decode.apply (required "email" string)



foo = do
    a <- Http.get url1
    b <- Http.get url2
    c <- Http.get url3

    return a + b + c

cmdRun : Parser {
    optimize : Bool,
    maxThreads : Nat,
    linker : [Surgical, Legacy]*,
    rocFilename : Str,
    argsForApp : List Str,
}
cmdRun = Parser.succeed with Parser.batch
    optimize << Arg.default Arg.flag False {
        long: "optimize",
        short: "O",
        help: "Optimize the compiled program to run faster. (Optimization takes time to complete.)",
    }
    maxThreads << Arg.default Arg.nat 0 {
        long: "max-threads",
        help: "Limit the number of threads (and hence cores) used during compilation.",
    }
    linker << Arg.default
        (Arg.alternatives [("surgical", Surgical), ("legacy", Legacy)]),
        Surgical,
        {
            long: "linker",
            help: "Sets which linker to use. The surgical linker is enabled by default.",
        }
    rocFilename << Arg.default Arg.str "main.roc" {
        help:"The .roc file to build",
    }
    argsForApp << Arg.passthrough {
        help: "Arguments to pass into the app being run, e.g. `roc run -- arg1 arg2`",
    }

    { optimize, maxThreads, linker, rocFilename, argsForApp }

args
|> about "Build a binary from the given .roc file, but don't run it"
|> arg ""
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())

    let app = Command::new("roc")
        .version(concatcp!(VERSION, "\n"))
        .about("Runs the given .roc file, if there are no compilation errors.\nUse one of the SUBCOMMANDS below to do something else!")
        .subcommand(Command::new(CMD_BUILD)
            .about("Build a binary from the given .roc file, but don't run it")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file to build")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
            )
        )
        .subcommand(Command::new(CMD_TEST)
            .about("Run all top-level `expect`s in a root module and any modules it imports.")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file for the root module")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME)
            )
            .arg(args_for_app.clone())
        )
        .subcommand(Command::new(CMD_REPL)
            .about("Launch the interactive Read Eval Print Loop (REPL)")
        )
        .subcommand(Command::new(CMD_RUN)
            .about("Run a .roc file even if it has build errors")
            .arg(flag_optimize.clone())
            .arg(flag_max_threads.clone())
            .arg(flag_opt_size.clone())
            .arg(flag_dev.clone())
            .arg(flag_debug.clone())
            .arg(flag_time.clone())
            .arg(flag_linker.clone())
            .arg(flag_precompiled.clone())
            .arg(flag_valgrind.clone())
            .arg(roc_file_to_run.clone())
            .arg(args_for_app.clone())
        )
        .subcommand(Command::new(CMD_FORMAT)
            .about("Format a .roc file using standard Roc formatting")
            .arg(
                Arg::new(DIRECTORY_OR_FILES)
                    .index(1)
                    .multiple_values(true)
                    .required(false)
                    .allow_invalid_utf8(true))
            .arg(
                Arg::new(FLAG_CHECK)
                    .long(FLAG_CHECK)
                    .help("Checks that specified files are formatted. If formatting is needed, it will return a non-zero exit code.")
                    .required(false),
            )
        )
        .subcommand(Command::new(CMD_VERSION)
            .about(concatcp!("Print the Roc compiler’s version, which is currently ", VERSION)))
        .subcommand(Command::new(CMD_CHECK)
            .about("Check the code for problems, but doesn’t build or run it")
            .arg(flag_time.clone())
            .arg(flag_max_threads.clone())
            .arg(
                Arg::new(ROC_FILE)
                    .help("The .roc file of an app to check")
                    .allow_invalid_utf8(true)
                    .required(false)
                    .default_value(DEFAULT_ROC_FILENAME),
            )
            )
        .subcommand(
            Command::new(CMD_DOCS)
                .about("Generate documentation for Roc modules (Work In Progress)")
                .arg(Arg::new(DIRECTORY_OR_FILES)
                    .multiple_values(true)
                    .required(false)
                    .help("The directory or files to build documentation for")
                    .allow_invalid_utf8(true)
                )
        )
        .trailing_var_arg(true)
        .arg(flag_optimize)
            .arg(flag_max_threads.clone())
        .arg(flag_opt_size)
        .arg(flag_dev)
        .arg(flag_debug)
        .arg(flag_time)
        .arg(flag_linker)
        .arg(flag_precompiled)
        .arg(flag_valgrind)
        .arg(roc_file_to_run.required(false))
        .arg(args_for_app);


cmdRun : Parser {
    optimize : Bool,
    maxThreads : Nat,
    linker : [Surgical, Legacy]*,
    rocFilename : Str,
    argsForApp : List Str,
}
cmdRun = with Arg.batch
    optimize := Arg.default Arg.flag False {
        long: "optimize",
        short: "-O",
        help: "Optimize the compiled program to run faster. (Optimization takes time to complete.)",
    }
    maxThreads := Arg.default Arg.nat 0 {
        long: "max-threads",
        help: "Limit the number of threads (and hence cores) used during compilation.",
    }
    linker := Arg.default
        (Arg.alternatives [("surgical", Surgical), ("legacy", Legacy)]),
        Surgical,
        {
            long: "linker",
            help:"Sets which linker to use. The surgical linker is enabled by default.",
        }
    rocFilename := Arg.default Arg.str "main.roc" {
        help: "The .roc file to build",
    }
    argsForApp := Arg.passthrough {
        long: "",
        help: "Arguments to pass into the app being run, e.g. `roc run -- arg1 arg2`",
    }

    Arg.succeed { optimize, maxThreads, linker, rocFilename, argsForApp }

cmdRun =
    Arg.succeed { Task.batch
        optimize <- Arg.default Arg.flag False {
            long: "optimize",
            short: "-O",
            help: "Optimize the compiled program to run faster. (Optimization takes time to complete.)",
        }
        maxThreads <- Arg.default Arg.nat 0 {
            long: "max-threads",
            help: "Limit the number of threads (and hence cores) used during compilation.",
        }
        linker <- Arg.default
            (Arg.alternatives [("surgical", Surgical), ("legacy", Legacy)]),
            Surgical,
            {
                long: "linker",
                help:"Sets which linker to use. The surgical linker is enabled by default.",
            }
        rocFilename <- Arg.default Arg.str "main.roc" {
            help: "The .roc file to build",
        }
        argsForApp <- Arg.passthrough {
            long: "",
            help: "Arguments to pass into the app being run, e.g. `roc run -- arg1 arg2`",
        }
    }
```

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser ->
  const (\_ -> \val -> \_ -> val)
  |> applyRev (scalar '[')
  |> applyRev parser
  |> applyRev (scalar ']')


betweenBraces2 : Parser a -> Parser { val: a }
betweenBraces2 = \parser ->
    Arg.succeed { Arg.batch with
        _ <- scalar '['
        val <- parser
        _ <- scalar ']'
    }

    task = with Task.await
        setup <- Http.get blah
        { val } <- Arg.succeed { with Arg.batch
            _ <- scalar '['
            val <- parser
            _ <- applyRev (scalar ']')
        }

    task = with Task.await
        setup <- Http.get blah
        { val } <- Parser.succeed { with Task.batch
            _ <- scalar '['
            val <- parser
            _ <- scalar ']'
        } |> Task.succeed

    task = with Task.await
        setup <- Http.get blah
        val <- with Task.batch
            _ := scalar '['
            val := parser
            _ := scalar ']'

            Task.succeed (Parser.succeed val)



betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser ->
  const (\_ -> \val -> \_ -> val)
  |> applyRev (scalar '[')
  |> applyRev parser
  |> applyRev (scalar ']')

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser ->
  const (\_ -> \val1 -> let x = val1 + 1 in \val2 -> let y = x + 1 in \_ -> val)
  |> applyRev (scalar '[')
  |> applyRev parser
  |> applyRev (scalar ']')

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    _ := scalar '['
    val1 := parser
    x = val1 + 1
    _ := scalar ','
    val2 := parser
    y = x + 1
    _ := scalar ']'

    Parser.succeed val

-- so this works but...it's useless. so I think the rule kinda has to be that you can't have
-- any `=` operators in between the `:=` operators.

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    _ := scalar '['
    val1 := parser
    _ := scalar ','
    val2 := parser
    _ := scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    _ << scalar '['
    val1 << parser
    _ << scalar ','
    val2 << parser
    _ << scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    {} <~ scalar '['
    val1 <~ parser
    {} <~ scalar ','
    val2 <~ parser
    {} <~ scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    {} :: scalar '['
    val1 :: parser
    {} :: scalar ','
    val2 :: parser
    {} :: scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    {} := scalar '['
    val1 := parser
    {} := scalar ','
    val2 := parser
    {} := scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    {} =| scalar '['
    val1 =| parser
    {} =| scalar ','
    val2 =| parser
    {} =| scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    {} |= scalar '['
    val1 |= parser
    {} |= scalar ','
    val2 |= parser
    {} |= scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    | {} = scalar '['
    | val1 = parser
    | {} = scalar ','
    | val2 = parser
    | {} = scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Task.batch
    | {} <- scalar '['
    | val1 <- parser
    | {} <- scalar ','
    | val2 <- parser
    | {} <- scalar ']'

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> Parser.succeed with Task.batch
    {} << scalar '['
    val1 << parser
    {} << scalar ','
    val2 << parser
    {} << scalar ']'

    y = x + 1
    x = val1 + 1

    val



cmdRun : Parser {
    optimize : Bool,
    maxThreads : Nat,
    linker : [Surgical, Legacy]*,
    rocFilename : Str,
    argsForApp : List Str,
}
cmdRun = with Arg.batch
    optimize << Arg.default Arg.flag False {
        long: "optimize",
        short: "-O",
        help: "Optimize the compiled program to run faster. (Optimization takes time to complete.)",
    }
    maxThreads << Arg.default Arg.nat 0 {
        long: "max-threads",
        help: "Limit the number of threads (and hence cores) used during compilation.",
    }
    linker << Arg.default
        (Arg.alternatives [("surgical", Surgical), ("legacy", Legacy)]),
        Surgical,
        {
            long: "linker",
            help: "Sets which linker to use. The surgical linker is enabled by default.",
        }
    rocFilename << Arg.default Arg.str "main.roc" {
        help:"The .roc file to build",
    }
    argsForApp << Arg.passthrough {
        help: "Arguments to pass into the app being run, e.g. `roc run -- arg1 arg2`",
    }

    Arg.succeed { optimize, maxThreads, linker, rocFilename, argsForApp }

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics << Http.get analyticsUrl
        searchResults << Http.get searchUrl
        summary << Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

thing = \contents -> with Task.await
    url1 <- File.read filename
    parser = Arg.succeed with Arg.batch
        foo << Arg.flag Arg.str
        bar << Arg.flag Arg.bool

        Str.concat foo bar

    File.write filename contents


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics <| Http.get analyticsUrl
        searchResults <| Http.get searchUrl
        summary <| Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics -< Http.get analyticsUrl
        searchResults -< Http.get searchUrl
        summary -< Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics | Http.get analyticsUrl
        searchResults | Http.get searchUrl
        summary | Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics |: Http.get analyticsUrl
        searchResults |: Http.get searchUrl
        summary |: Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics |= Http.get analyticsUrl
        searchResults |= Http.get searchUrl
        summary |= Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics <~ Http.get analyticsUrl
        searchResults <~ Http.get searchUrl
        summary <~ Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics := Http.get analyticsUrl
        searchResults := Http.get searchUrl
        summary := Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics <-| Http.get analyticsUrl
        searchResults <-| Http.get searchUrl
        summary <-| Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics <- Http.get analyticsUrl
        searchResults <- Http.get searchUrl
        summary <- Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics ~= Http.get analyticsUrl
        searchResults ~= Http.get searchUrl
        summary ~= Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str


writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch
        analytics |< Http.get analyticsUrl
        searchResults |< Http.get searchUrl
        summary |< Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch [
            analytics <- Http.get analyticsUrl
            searchResults <- Http.get searchUrl
            summary <- Http.get summaryUrl
        ]

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch (
            analytics <- Http.get analyticsUrl
            searchResults <- Http.get searchUrl
            summary <- Http.get summaryUrl
        )

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with Task.batch {
            analytics <- Http.get analyticsUrl,
            searchResults <- Http.get searchUrl,
            summary <- Http.get summaryUrl,
        }

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    str <- Task.succeed with { Task.batch
            analytics <- Http.get analyticsUrl
            searchResults <- Http.get searchUrl
            summary <- Http.get summaryUrl
        }

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    File.write outputFilename str

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> Parser.succeed with Parser.batch {
        _ <- scalar '['
        val1 <- parser
        _ <- scalar ','
        val2 <- parser
        _ <- scalar ']'
    }

    y = x + 1
    x = val1 + 1

    Parser.succeed val

betweenBraces2 : Parser a -> Parser a
betweenBraces2 = \parser -> with Parser.batch
    _ << scalar '[',
    val1 << parser,
    _ << scalar ',',
    val2 << parser,
    _ << scalar ']',

    y = x + 1
    x = val1 + 1

    Parser.succeed val

writeToDisk = \searchUrl, outputFilename -> with Task.await
    analyticsUrl <- File.read analyticsFilename

    foo = "\(analyticsUrl)!"

    str <- with Task.await
        analytics <- Http.get analyticsUrl
        searchResults <- Http.get searchUrl
        summary <- Http.get summaryUrl

        Task.succed (Str.join [summary, analytics, Str.joinWith ", " searchResults])

    str <- Task.succed with Task.batch
        analytics << Http.get analyticsUrl
        searchResults << Http.get analytics
        summary << Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    str <- Task.succeed with Task.batch
        analytics << Http.get analyticsUrl
        searchResults << Http.get searchUrl
        summary << Http.get summaryUrl

        Str.join [summary, analytics, Str.joinWith ", " searchResults]

    foo = "\(str)!"

    File.write outputFilename str

Parser a := [
    Succeed a,
    Lazy ({} -> a)
]

andMap : Parser a, Parser (a -> b) -> Parser b
andMap = \@Parser parser, @Parser mapper ->
    @Parser when mapper is
        Succeed fn ->
            when parser is
                Succeed a -> Lazy \{} -> fn a
                Lazy thunk -> Lazy \{} -> fn (thunk {})

        Lazy thunk ->
            when parser is
                Succeed a -> Lazy \{} -> (thunk {}) a
                Lazy innerThunk -> Lazy \{} -> (thunk {}) (innerThunk {})

