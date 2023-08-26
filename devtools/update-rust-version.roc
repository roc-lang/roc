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
        pf.Stderr,
        pf.Cmd,
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

        {} <- Stdout.line "Checking current rust versions..." |> await
        {stable, nightly} <- getCurrentVersionsFromToml |> await
        {} <- Stdout.line "Found:\n\tstable: \(versionToStr stable)\n\tnightly: from \(dateToStr nightly)" |> await

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

        {} <- updateNightly |> await
        Task.ok {}
    else
        Task.err NotInRocDir

getCurrentVersionsFromToml : Task { stable : RustVersion, nightly : Date } _
getCurrentVersionsFromToml =
    fileContent <-
        Path.fromStr "rust-toolchain.toml"
        |> File.readUtf8
        |> await

    Str.split fileContent "\n"
    |> List.walk { stable: versionFromStr "1.70.0", nightly: { year: 2023, month: 1, day: 1 } } \state, line ->
        when Str.split line "\"" is
            ["channel = ", version, ..] ->
                { state & stable: versionFromStr version }

            ["# channel = ", version, ..] ->
                # here, `version` should look like "nightly-2023-04-15"
                # so split by "-" and keep natural numbers
                dateParts = Str.split version "-" |> List.keepOks Str.toNat
                when dateParts is
                    [year, month, day] -> { state & nightly: { year, month, day } }
                    _ -> state

            _ -> state
    |> Task.ok

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

padLeftWithZero : Str -> Str
padLeftWithZero = \str ->
    if Str.countUtf8Bytes str == 1 then
        Str.concat "0" str
    else
        str

# TODO dateFromStr
Date : { year : Nat, month : Nat, day : Nat }
dateToStr : Date -> Str
dateToStr = \{ year, month, day } ->
    yearStr = Num.toStr year
    monthStr = Num.toStr month |> padLeftWithZero
    dayStr = Num.toStr day |> padLeftWithZero
    "\(yearStr)-\(monthStr)-\(dayStr)"

# stable rust version
RustVersion := Str

versionFromStr : Str -> RustVersion
versionFromStr = \str ->
    @RustVersion str

versionToStr : RustVersion -> Str
versionToStr = \@RustVersion str ->
    str

# TODO: maybe it could be possible to make some assertions here in case the format changes
parseVersionFromNix : Str -> Result RustVersion _
parseVersionFromNix = \nixStr ->
    # nixStr: {v="1.70.0-nightly";d="2023-04-15";r=5;p=5;cargo={_0="6 ...

    { before: _, after } <- Str.splitFirst nixStr "\"" |> Result.try
    # after: 1.70.0-nightly";d="2023-04-15";r=5;p=5;cargo={_0="6 ...

    { before, after: _ } <- Str.splitFirst after "-" |> Result.try
    # before: 1.70.0

    Ok (versionFromStr before)

getNightlyVersion : Date -> Task RustVersion _
getNightlyVersion = \date ->
    nightlyFilePrefix = "rust-overlay/manifests/nightly/"
    nightlyFilePath =
        Str.joinWith [nightlyFilePrefix, "\(Num.toStr date.year)/", dateToStr date, ".nix"] ""
        |> Path.fromStr

    nighltyFileContents <- File.readUtf8 nightlyFilePath |> await
    result =
        nighltyFileContents
        |> parseVersionFromNix

    when result is
        Ok version -> Task.ok version
        Err err ->
            {} <- Stderr.line "Failed to parse nightly version from file \(Path.display nightlyFilePath)" |> await
            Task.err err

cloneRustOverlay : Task {} _
cloneRustOverlay =
    url = "https://github.com/oxalica/rust-overlay.git"
    {} <- Stdout.line "Cloning \(url) into ./rust-overlay" |> await
    Cmd.new "git"
    |> Cmd.args ["clone", "--depth=1", url]
    |> Cmd.status
    |> Task.onErr \err -> Stderr.line "Failed to clone \(url)"

rmRustOverlay : Task {} _
rmRustOverlay =
    path = "./rust-overlay"
    {} <- Stdout.line "Removing \(path)" |> await
    Cmd.new "rm"
    |> Cmd.args ["-fr", path]
    |> Cmd.status
    |> Task.onErr \err -> Stderr.line "Failed to remove \(path)"

updateNightly : Task {} _
updateNightly =
    {} <- cloneRustOverlay |> await
    version <- getNightlyVersion { year: 2023, month: 4, day: 15 } |> await
    {} <- Stdout.line (versionToStr version) |> await
    {} <- rmRustOverlay |> await
    Task.ok {}

