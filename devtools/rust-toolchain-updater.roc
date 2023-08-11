#!/usr/bin/env roc
app "rust-toolchain-updater"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Task.{ Task, await },
        pf.File,
        pf.Path,
        pf.Env,
        pf.Process,
        pf.Stdout,
    ]
    provides [main] to pf

updateDotToml = 
    fileName = "rust-toolchain.toml"
    version = "1.71.0"
    path = Path.fromStr fileName
    contents <- File.readUtf8 path |> await
    Str.split contents "\n"
    |> List.map \line ->
        split = Str.split line " "
        when split is
            ["channel", ..] -> "channel = \"" |> Str.concat version |> Str.concat "\""
            _ -> line
    |> Str.joinWith "\n"
    |> \x -> File.writeUtf8 (Path.fromStr fileName) x

run =
    cwd <- Env.cwd |> await
    basename <- Path.display cwd
        |> Str.split "/"
        |> List.last
        |> Task.fromResult
        |> await
    if basename == "roc" then
        updateRustToolchain
    else
        Task.fail NotInRocDir

main =
    Task.attempt run \result ->
        when result is
            Ok {} -> Process.exit 0
            Err err ->
                msg =
                    when err is
                        NotInRocDir -> "this script should be run from inside roc git repository"
                        _ -> ""
                {} <- Stdout.line msg |> await
                Process.exit 1
