app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    build: "../../examples/build-helpers/main.roc",
}

import cli.Task exposing [Task]
import cli.Path
import cli.Env
import cli.Cmd
import build.Help

cargoTargetLibraryPath = "target/debug/libhost.a"
cargoTargetExecutablePath = "target/debug/host"
stubbedDynamicLibraryPath = \target -> "platform/libapp$(Help.dynamicLibraryExtension target)"

main =

    # use ENV VARs for easier automation from within test runner
    roc = Env.var "ROC" |> Task.mapErr! \_ -> EnvionmentVariableNotSet "ROC"

    # get the current OS and ARCH
    target = getTarget!

    # the prebuilt binary `macos-arm64.a` changes based on target
    prebuiltBinaryPath = "platform/$(Help.prebuiltBinaryName target)"

    when Path.isFile (Path.fromStr prebuiltBinaryPath) |> Task.result! is
        Ok _ ->
            Task.ok {} # the prebuilt binary already exists... no need to rebuild

        Err _ ->
            # build the host
            Cmd.exec "cargo" ["build"]
                |> Task.mapErr! ErrBuildingHost

            # copy pre-built binary into platform for legacy linking
            Cmd.exec "cp" ["-f", cargoTargetLibraryPath, prebuiltBinaryPath]
                |> Task.mapErr! ErrCopyPrebuiltBinary

            # build stubbed dynamic library
            Cmd.exec roc [
                "build",
                "--lib",
                "platform/libapp.roc",
                "--output",
                stubbedDynamicLibraryPath target,
            ]
            |> Task.mapErr! ErrBuildingStubbedDylib

            # pre-process host for surgical linking
            Cmd.exec roc [
                "preprocess-host", 
                cargoTargetExecutablePath, 
                "platform/main.roc",
                stubbedDynamicLibraryPath target,
            ] 
            |> Task.mapErr! ErrPreProcessingHost

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

