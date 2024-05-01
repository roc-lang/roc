app ""
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
        build: "../../../../../examples/build-helpers/main.roc",
    }
    imports [cli.Task.{ Task }, cli.Cmd, build.Help]
    provides [main] to cli

main =

    Cmd.exec! "zig" ["build"]

    target = getTarget!
    
    Cmd.exec! "cp" ["-f", "zig-out/lib/libfibonacci-platform.a", Help.prebuiltBinaryName target]

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

