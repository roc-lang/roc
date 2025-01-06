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
    err <- run
        |> Task.onErr

    msg =
        when err is
            NotInRocDir ->
                "This script should be run from the root of the roc repository folder."

            CloneRustOverlayErr (IOError str) ->
                "Failed to clone oxalica/rust-overlay IOError:\n\t\(str)."

            CloneRustOverlayErr _ ->
                "Failed to clone oxalica/rust-overlay."

            RemoveRustOverlayErr (IOError str) ->
                "Failed to remove oxalica/rust-overlay:\n\t\(str)."

            RemoveRustOverlayErr _ ->
                "Failed to clone oxalica/rust-overlay."

            FailedParsingNightlyFromNix date WrongFormat ->
                "Failed to parse nightly-\(dateToStr date) with error:\n\tWrongFormat."

            FailedParsingNightlyFromNix date _ ->
                "Failed to parse nightly-\(dateToStr date) with error:\n\tUnknown."

            FileReadErr path _ ->
                "Failed to read file:\n\t\(Path.display path)."

            FileReadUtf8Err path (BadUtf8 _ _) ->
                "Failed to read file:\n\t\(Path.display path)\n\tError: Not a valid Utf8 string."

            FileWriteErr path _ ->
                "Failed to write to file:\n\t\(Path.display path)"

            CwdUnavailable ->
                "Information about current working directory is not available."

            # This error is currently impossible, Str.split never returns the empty list.
            PathListEmpty path ->
                "I tried to get the name of the current directory, so I split up  \(Path.display path) and tried to get the last element but the split list was empty."

    {} <- Stderr.line "Script failed:\n\t\(msg)" |> await

    Task.err 1

run : Task {} _
run =
    currentDirPath <- Env.cwd |> await
    currentDir <- Path.display currentDirPath
        |> Str.split "/"
        |> List.last
        |> Result.mapErr \_ -> PathListEmpty currentDirPath
        |> Task.fromResult
        |> await

    if currentDir == "roc" then
        # TODO request new Rust version from user using Stdin.line
        newRustVersion = "1.71.0"
        # TODO start with checking if this version is available on Nix, see
        # `assert pkgs.lib.assertMsg rustVersionsMatch ...` in default.nix

        {} <- log "Extracting current rust version from rust-toolchain.toml..." |> await
        { stable: currentStable, nightly: currentNightly } <- getCurrentVersionsFromToml |> await

        {} <- log "Found versions:\n\t- stable: \(versionToStr currentStable)\n\t- nightly: nightly-\(dateToStr currentNightly)"  |> await
        {} <- log "Searching for nightly matching stable-\(newRustVersion)..." |> await
 
        newNightly <-
            findMatchingNigthly (versionFromStr newRustVersion) currentNightly
            |> await

        {} <- log "Found most recent nightly matching \(newRustVersion): nightly-\(dateToStr newNightly)" |> await

        TODO can we avoid boiler plate of `{} <-` and `await` here?
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

findMatchingNigthly : RustVersion, Date -> Task Date _
findMatchingNigthly = \desiredVersion, startingDate ->

    checkFile = \fileDate ->
        fileContents <- getNightlyFile fileDate |> await
        version <- parseVersionFromNix fileContents
            |> Task.fromResult
            |> Task.mapErr \err -> FailedParsingNightlyFromNix fileDate err
            |> await

        if versionsEqual version desiredVersion then
            Done fileDate |> Task.ok
        else
            # TODO: find a better and correct way to get next date
            # maybe list all files under 'manifests/nightly/{year}/'
            # and check them one by one
            nextDate fileDate |> Step |> Task.ok

    {} <- cloneRustOverlay |> await
    nightly <- Task.loop startingDate checkFile |> await
    {} <- rmRustOverlay |> await

    Task.ok nightly

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

nextDate : Date -> Date
nextDate = \{ year, month, day } ->
    # TODO this is totally wrong, to be fixed
    { year, month, day: day + 1 }

# stable rust version
RustVersion := Str

versionFromStr : Str -> RustVersion
versionFromStr = \str ->
    @RustVersion str

versionToStr : RustVersion -> Str
versionToStr = \@RustVersion str ->
    str

versionsEqual : RustVersion, RustVersion -> Bool
versionsEqual = \l, r ->
    versionToStr l == versionToStr r

# TODO: maybe it could be possible to make some assertions here in case the format changes
parseVersionFromNix : Str -> Result RustVersion _
parseVersionFromNix = \nixStr ->
    # nixStr: {v="1.70.0-nightly";d="2023-04-15";r=5;p=5;cargo={_0="6 ...

    Str.splitFirst nixStr "\""
    |> Result.try
        # after: 1.70.0-nightly";d="2023-04-15";r=5;p=5;cargo={_0="6 ...
        \{ before: _, after } -> Str.splitFirst after "-"
    |> Result.mapErr \_ -> WrongFormat
    |> Result.map
        # before: 1.70.0
        \{ before, after: _ } -> versionFromStr before

getNightlyFile : Date -> Task Str _
getNightlyFile = \date ->
    nightlyFolder = "rust-overlay/manifests/nightly/"

    nightlyFilePath =
        Str.joinWith
            [nightlyFolder, Num.toStr date.year, "/", dateToStr date, ".nix"]
            ""
        |> Path.fromStr

    File.readUtf8 nightlyFilePath

cloneGitRepo : Str -> Task {} _
cloneGitRepo = \url ->
    Cmd.new "git"
        |> Cmd.args ["clone", "--depth=1", url]
        |> Cmd.status

cloneRustOverlay : Task {} _
cloneRustOverlay =
    url = "https://github.com/oxalica/rust-overlay.git"

    {} <- log "Cloning \(url) into ./rust-overlay" |> await
    err <-
        cloneGitRepo url
        |> Task.onErr

    {} <- log "Trying to remove ./rust-overlay" |> await

    {} <-
        rmRustOverlay
        |> Task.mapErr (\_ -> CloneRustOverlayErr err)
        |> await

    {} <- log "Trying to clone again\n" |> await

    cloneGitRepo url
    |> Task.mapErr CloneRustOverlayErr

rmRustOverlay : Task {} _
rmRustOverlay =
    path = "./rust-overlay"
    {} <- log "Removing \(path)..." |> await

    Cmd.new "rm"
    |> Cmd.args ["-rf", path]
    |> Cmd.status
    |> Task.mapErr RemoveRustOverlayErr

log = \str ->
    Stdout.line "> \(str)\n"

