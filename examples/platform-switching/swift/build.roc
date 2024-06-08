app ""
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
        build: "../../../examples/build-helpers/main.roc",
    }
    imports [cli.Task.{ Task }, cli.Path, cli.Cmd, build.Help]
    provides [main] to cli

main =

    # get the current OS and ARCH
    target = getTarget!

    # the prebuilt binary `macos-arm64.a` changes based on target
    prebuiltBinaryPath = "platform/$(Help.prebuiltBinaryName target)"

    when Path.isFile (Path.fromStr prebuiltBinaryPath) |> Task.result! is 
        Ok _ ->
            Task.ok {} # the prebuilt binary already exists... no need to rebuild
        Err _ ->
            # build the host
            Cmd.exec "swift" ["build","-c","release"]
            |> Task.mapErr! ErrBuildingHost

            # copy pre-built binary into platform
            Cmd.exec "cp" ["-f", "./.build/release/libhost.a", prebuiltBinaryPath]
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

