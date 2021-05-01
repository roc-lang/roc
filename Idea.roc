
Inputs :
    {
        watches : List Str,
        generates : List Str,
        runs : List Path -> Effect (List Problem)
    }

Problem : [ Error Str, Warning Str ]

run = \env ->
    testDepsDat =  "\(env.cacheDir)/tests/**/*.deps.dat"
    testOutputLog =  "\(env.dir)/testOutput.log"

    Task.succeed
        [
            subcmd "build"
                [
                    cmd "cli"
                        {
                            watches: [ "cli/src/**/*.rs" ],
                            generates: [ "roc" ],
                            runs: \filenames -> buildCli
                        }
                    cmd ""
                        {
                            watches: [ "*/src/**/*.rs" ],
                            generates: [ "roc" ]
                            runs: \filenames -> buildAll
                        }
                ],
            depsFor "module in roc/___ package"
                {
                    # Whenever any of these changes, we will recompute its deps
                    appliesTo: [ "packages/**/*.roc" ],

                    # Run this command passing the individual file, to determine
                    # what the file's (direct) dependencies are. (This will
                    # recursively invoke the command as much as necessary to
                    # determine transitive deps that aren't yet known.) The
                    # command should print one direct dep filename per line to stdout.
                    #
                    # Also, if the command referenced by this string (as
                    # determined by `which`) changes, this will be re-run.
                    #
                    # Executing any process takes a minimum of 2ms, so we will
                    # invoke these as concurrently as possible!
                    getDirectDeps: exec "get-roc-direct-deps.pl" [ Arg.path Path.filename ]

                    # Whenever a file changes (or any of its dependencies
                    # changes, including transitive dependencies), run this.
                    #
                    # Also, if the command referenced by the first string (as
                    # determined by `which`) changes, this will be re-run.
                    onFileOrDepChange:
                        # keep only the package dir; discard the src/**/*.roc
                        cwd = Path.truncateToGlob Path.filename "packages/*"

                        # cd into the package directory and run `roc build`
                        Cmd.execWith "roc" [ Arg.str "build" ] { cwd }

                }
            depsFor "Rust Test"
                {
                    # Whenever any of these changes, we will recompute its deps
                    appliesTo: [ "*/tests/**/*.rs" ],

                    # Run this plugin passing the individual file, to determine
                    # what the file's (direct) dependencies are. (This will
                    # recursively invoke the plugin as much as necessary to
                    # determine transitive deps that aren't yet known.) The
                    # plugin should return a RocList of direct dep filenames.
                    #
                    # PLUGIN DESIGN:
                    #
                    # When you give it a .roc file for a plugin, we go and find
                    # all of *its* deps, so we know when it changes and can
                    # rebuild both the plugin *and* all the things that are
                    # using it. Once we've rebuilt it, we now have the .o file
                    # and can ld it in, or dlopen it if running in watch mode!
                    #
                    # This in turn means that once we've compiled this config
                    # file, as well as all the plugins, we have one final
                    # executable that can be run (and re-run) directly without
                    # any need for recompilation - unless anything changes, of
                    # course, in which case it'll be automatically rebuilt.
                    #
                    # Thus, unless you change your build config, you're basically
                    # just running one executable every time you do a build -
                    # so that's about 2ms overhead. No parsing even.
                    getDirectDeps: plugin "FindRustDeps.roc" [ Arg.path Path.filename ]

                    # Whenever a file changes (or any of its dependencies
                    # changes, including transitive dependencies), run this.
                    #
                    # Also, if the command referenced by the first string (as
                    # determined by `which`) changes, this will be re-run.
                    onFileOrDepChange:
                        exec "cargo"
                            [
                                Arg.str "test",
                                Arg.path Path.filename
                            ]
                }
           #cmd "test"
           #    {
           #        watches: [ "*/tests/**/*.rs" ],
           #        generates: [ testDepsDat ]
           #        runs:
           #            [
           #                Effect.forEach # Effect (List a), Effect (a -> Effect b) -> Effect (List b)
           #                    Effect.getChangedFiles # Effect (List Str)
           #                    Effect.execWithArg "build-deps.sh", # Str -> Effect (Str -> Outcome)

           #                # TODO need some built-in way to express "run this external command
           #                # which will produce a newline-delimited list of all the dependencies
           #                # of this file, so we can build a graph of them all.
           #            ]

           #          \filenames ->
           #            # Rebuild test deps for only the filenames that changed,
           #            # plus the files that depend on those (according to
           #            # existing tests/**/*.deps.dat files).
           #            testDeps <- rebuildTestDeps filenames
           #            _ <- writeTestDeps testDeps testDepsDat

           #            # Reads various testDeps.dat files to determine which
           #            # tests changed, and thus which tests to run.
           #            runTests
           #    },
           #task "Rebuild packages"
           #    {
           #        watches: [ "packages/**/*" ],
           #        generates: [], # We don't care about what this generates
           #        runs: \filenames ->
           #            # getUniqueDirs is a Task because it resolves symlinks
           #            packageDirs <- getUniqueDirs filenames

           #            buildEach packageDirs
           #    },
        ]
