app ""
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
        build: "../../../../examples/build-helpers/main.roc",
    }
    imports [cli.Task.{ Task }, cli.Path, cli.Env, cli.Cmd, build.Help]
    provides [main] to cli

main =

    # use ENV VARs for easier automation from within test runner
    roc = Env.var "ROC" |> Task.mapErr! \_ -> EnvionmentVariableNotSet "ROC"
    glue = Env.var "ZIG_GLUE" |> Task.mapErr! \_ -> EnvionmentVariableNotSet "ZIG_GLUE"

    # get the current OS and ARCH
    target = getTarget!

    # the prebuilt binary `macos-arm64.a` changes based on target
    prebuiltBinaryPath = Help.prebuiltBinaryName target

    when Path.isFile (Path.fromStr prebuiltBinaryPath) |> Task.result! is 
        Ok _ ->
            Task.ok {} # the prebuilt binary already exists... no need to rebuild
        Err _ -> 
            # generate glue for the host
            Cmd.exec roc ["glue", glue, "glue/", "platform-glue-workaround.roc"]
            |> Task.mapErr! ErrGeneratingGlue

            # build the host
            Cmd.exec "zig" ["build"]
            |> Task.mapErr! ErrBuildingHost

            # copy pre-built binary into platform
            Cmd.exec "cp" ["-f", "zig-out/lib/libhost.a", prebuiltBinaryPath]
            |> Task.mapErr! ErrCopyPrebuiltBinary

getTarget : Task Help.RocTarget _
getTarget =

    arch =
        Cmd.new "uname"
        |> Cmd.arg "-m"
        |> Cmd.output
        |> Task.map .stdout
        |> Task.map Help.archFromStr
        |> Task.mapErr! \err -> ErrGettingNativeArch (Inspect.toStr err)

    os =
        Cmd.new "uname"
        |> Cmd.arg "-s"
        |> Cmd.output
        |> Task.map .stdout
        |> Task.map Help.osFromStr
        |> Task.mapErr! \err -> ErrGettingNativeOS (Inspect.toStr err)
        
    Help.rocTarget { os, arch } |> Task.fromResult!

