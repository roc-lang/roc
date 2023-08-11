#!/usr/bin/env roc
app "rust-toolchain-updater"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Task.{ Task, await },
        pf.File,
        pf.Path,
        pf.Process,
    ]
    provides [main] to pf

main =
    fileName = "rust-toolchain.toml"
    version = "1.71.0"
    path = Path.fromStr fileName
    task =
        contents <- File.readUtf8 path |> await
        Str.split contents "\n"
        |> List.map \line ->
            split = Str.split line " "
            when split is
                ["channel", ..] -> "channel = \"" |> Str.concat version |> Str.concat "\""
                _ -> line
        |> Str.joinWith "\n"
        |> \x -> File.writeUtf8 (Path.fromStr fileName) x

    Task.attempt task \result ->
        when result is
            Ok {} -> Process.exit 0
            Err _err -> Process.exit 1
