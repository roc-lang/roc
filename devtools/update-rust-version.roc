# !/usr/bin/env roc
#
# run this script from the root of the roc repo
#
# WIP: this script will eventually do all the steps described at the top of rust-toolchain.toml
#
app "update-rust-version"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Task.{ Task, await },
        pf.File,
        pf.Path.{ Path },
        pf.Env,
        pf.Stdout,
    ]
    provides [main] to pf

# TODO add some logging, e.g. reading file <FILENAME>, writing file <FILENAME>

main : Task {} I32
main =
    runResult <- Task.attempt run

    when runResult is
        Ok {} ->
            Task.ok {}

        Err err ->
            msg =
                when err is
                    NotInRocDir ->
                        "This script should be run from the root of the roc repository folder."
                    _ ->
                        "" # TODO improve error message

            {} <- Stdout.line "Script failed:\n\t\(msg)" |> await

            Task.err 1

run : Task {} _
run =
    currentDirPath <- Env.cwd |> await
    currentDir <- Path.display currentDirPath
        |> Str.split "/"
        |> List.last
        |> Task.fromResult
        |> await

    if currentDir == "roc" then
        newRustVersion = "1.71.0"

        # TODO can we avoid boiler plate of `{} <-` and `await` here?
        {} <-
            Path.fromStr "rust-toolchain.toml"
            |> updateToml newRustVersion
            |> await

        {} <-
            Path.fromStr "examples/platform-switching/rust-platform/rust-toolchain.toml"
            |> updateToml newRustVersion
            |> await

        {} <-
            Path.fromStr "Earthfile"
            |> updateEarthFile newRustVersion
            |> await

        Task.ok {}
    else
        Task.err NotInRocDir

# TODO try to avoid repition of same code in updateToml and updateEarthFile
updateToml : Path, Str -> Task {} _
updateToml = \tomlPath, newRustVersion ->

    fileContent <-
        File.readUtf8 tomlPath |> await

    Str.split fileContent "\n"
    |> List.map \line ->
        split = Str.split line " "

        when split is
            ["channel", ..] ->
                "channel = \""
                |> Str.concat newRustVersion
                |> Str.concat "\"" # TODO don't discard rest of old line

            _ ->
                line

    |> Str.joinWith "\n"
    |> \newFileContent -> File.writeUtf8 tomlPath newFileContent

updateEarthFile : Path, Str -> Task {} _
updateEarthFile = \path, newRustVersion ->

    fileContent <-
        File.readUtf8 path |> await

    Str.split fileContent "\n"
    |> List.map \line ->
        split = Str.split line " "

        when split is
            ["FROM", ..] ->
                "FROM rust:"
                |> Str.concat newRustVersion
                |> Str.concat "-slim-buster" # TODO don't discard rest of old line

            _ ->
                line

    |> Str.joinWith "\n"
    |> \newFileContent -> File.writeUtf8 path newFileContent
