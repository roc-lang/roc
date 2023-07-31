app "fuzz-glue"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Arg,
        pf.Command,
        Random.{ Generator },
        "static/app-template.roc" as appTemplate : Str,
        "static/platform-template.roc" as platformTemplate : Str,
        "static/host-template.rs" as hostTemplate : Str,
        "static/host.c" as hostC : Str,
        "static/build.rs" as buildRs : Str,
        "static/Cargo.toml" as cargoToml : Str,
    ]
    provides [main] to pf

genAppPath = "generated/app.roc"
genPlatPath = "generated/platform.roc"
genHostLibPath = "generated/src/lib.rs"

# TODO just cp all this stuff over
genCargoTomlPath = "generated/Cargo.toml"
genHostCPath = "generated/host.c"
genBuildRsPath = "generated/build.rs"

defaultSeed : U64
defaultSeed = 1234567


Type : [
    Str,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    ListStr,
]

typeToRocStr : Type -> Str
typeToRocStr = \type ->
    when type is
        Str -> "Str"
        U8 -> "U8"
        I8 -> "I8"
        U16 -> "U16"
        I16 -> "I16"
        U32 -> "U32"
        I32 -> "I32"
        U64 -> "U64"
        I64 -> "I64"
        U128 -> "U128"
        I128 -> "I128"
        ListStr -> "List Str"

genType : Generator Type
genType =
    Random.uniform Str [U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, ListStr]

genArgsAndResult : Generator (List Type, Type)
genArgsAndResult =
    args, ret <- Random.map2 (Random.list genType 12) genType
    (args, ret)

main : Task {} U32 # TODO Return I32 after upgrading basic-cli
main =
    # We expect 1 CLI arg, namely the RNG seed
    cliArgs <- Arg.list |> Task.await

    seed =
        List.get cliArgs 1
        |> Result.try Str.toU64
        |> Result.withDefault defaultSeed
        |> Random.seed

    count <- Task.loop { seed, count: 0 } loop |> Task.await
    countStr = Num.toStr count
    pluralized =
        if count == 1 then
            "test"
        else
            "tests"

    Stdout.line "\(countStr) randomly-generated \(pluralized) passed."

loop : { seed : Random.Seed, count : U64 } -> Task [Step { seed : Random.Seed, count : U64 }, Done U64] U32
loop = \{ seed, count } ->
    # Randomly generate main's arguments and return type from the provided seed
    ((mainArgs, mainRetType), nextSeed) = Random.step seed genArgsAndResult

    mainType <- gen mainArgs mainRetType |> Task.await

    glueOut <-
        Command.new "roc"
        |> Command.arg "glue"
        |> Command.arg "../../src/RustGlue.roc"
        |> Command.arg "generated/glue/"
        |> Command.arg "generated/platform.roc"
        |> Command.output
        |> Task.await

    # Fail if `roc glue` failed
    {} <-
        Task.fromResult glueOut.status
        |> Task.onFail (onSpawnFail "roc glue" glueOut.stderr)
        |> Task.await

    appOut <-
        Command.new "roc"
        |> Command.arg "dev"
        |> Command.arg "generated/app.roc"
        |> Command.output
        |> Task.await

    # Fail if `roc dev` failed
    {} <-
        Task.fromResult glueOut.status
        |> Task.onFail (onSpawnFail "roc dev generated/app.roc" appOut.stderr)
        |> Task.await

    if (Str.fromUtf8 appOut.stdout == Ok "Assertion passed!\n") && (Str.fromUtf8 appOut.stderr == Ok "🔨 Rebuilding platform...\n") then
        {} <- Stdout.line "✅ main : \(mainType)" |> Task.await
        # Continue fuzzing forever!!!
        Task.succeed (Step { seed: nextSeed, count: count + 1 })
    else
        stdoutStr = Str.fromUtf8 appOut.stdout |> Result.withDefault "<bad utf8>" # TODO Str.fromUtf8Lossy
        stderrStr = Str.fromUtf8 appOut.stderr |> Result.withDefault "<bad utf8>" # TODO Str.fromUtf8Lossy
        {} <- Stdout.line "❌ main : \(mainType)\nTest failed with stdout: \"\(stdoutStr)\" and stderr \"\(stderrStr)\"" |> Task.await
        seedStr = Num.toStr (Random.seedToU64 seed)

        {} <- Stdout.line "\nThe generated files which failed the test can be found in: generated/\n\nTo reproduce this failed test, pass this seed: \(seedStr)" |> Task.await

        Task.succeed (Done count)

onSpawnFail : Str, List U8 -> (_ -> Task * U32) # TODO "_" should be Command.Error once this lands: https://github.com/roc-lang/basic-cli/pull/91
onSpawnFail = \cmdName, stderrUtf8 -> \err ->
    (exitCode, msg) =
        when err is
            ExitCode code ->
                codeStr = Num.toStr code
                (Num.toU32 code, "exited with code \(codeStr)")

            KilledBySignal ->
                (1, "was killed by a signal")

            IOError ioErrStr ->
                (1, "encountered an I/O error: \"\(ioErrStr)\"")

    stderrStr = Str.fromUtf8 stderrUtf8 |> Result.withDefault "<invalid UTF-8>" # TODO use Str.fromUtf8Lossy once that exists

    {} <- Stdout.line "`\(cmdName)` \(msg). Its stderr was:\n\n\(stderrStr)" |> Task.await
    Task.fail exitCode

gen : List Type, Type -> Task Str U32
gen = \mainArgTypes, mainRetType ->
    # TODO rm -rf generated/
    # TODO mkdir -p generated/src/

    mainArgVals = List.map mainArgTypes valFromType

    # Actually use all the arguments to determine the return value,
    # so we have some way to tell if they made it through correctly.
    { roc: mainBody, rust, expected } = genHelp mainArgVals mainRetType

    mainRetTypeStr = typeToRocStr mainRetType
    mainType =
        if List.isEmpty mainArgTypes then
            mainRetTypeStr
        else
            mainArgTypes
            |> List.map typeToRocStr
            |> Str.joinWith ", "
            |> Str.concat " -> \(mainRetTypeStr)"

    mainLambda =
        if List.isEmpty mainArgTypes then
            ""
        else
            # Declare all the arguments
            argStrings =
                List.mapWithIndex mainArgTypes \_arg, index ->
                    indexStr = Num.toStr index
                    "arg\(indexStr)"

            # e.g. `\arg0, arg1, arg2 ->`
            "\\"
            |> Str.concat (Str.joinWith argStrings ", ")
            |> Str.concat " ->"

    mainForHostBody =
        if List.isEmpty mainArgTypes then
            "main"
        else
            # We have to eta-expand to avoid a compiler bug
            argStrings =
                List.mapWithIndex mainArgTypes \_arg, index ->
                    indexStr = Num.toStr index
                    "arg\(indexStr)"

            # e.g. `\arg0, arg1, arg2 -> main arg0 arg1 arg2
            "\\"
            |> Str.concat (Str.joinWith argStrings ", ")
            |> Str.concat " -> main "
            |> Str.concat (Str.joinWith argStrings " ")

    genAppContent =
        appTemplate
        |> Str.replaceFirst "# {{ mainType }}" mainType
        |> Str.replaceFirst "# {{ mainBody }}" mainBody
        |> Str.replaceFirst "# {{ mainLambda }}" mainLambda

    genPlatContent =
        platformTemplate
        |> Str.replaceFirst "# {{ mainType }}" mainType
        |> Str.replaceFirst "# {{ mainForHostType }}" mainType
        |> Str.replaceFirst "# {{ mainForHostBody }}" mainForHostBody

    genHostContent =
        hostTemplate
        |> Str.replaceFirst "// {{ mainForHostArgs }}" rust
        |> Str.replaceFirst "// {{ mainForHostExpected }}" expected

    task =
        {} <- File.writeUtf8 (Path.fromStr genAppPath) genAppContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genPlatPath) genPlatContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostLibPath) genHostContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostCPath) hostC |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genBuildRsPath) buildRs |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genCargoTomlPath) cargoToml |> Task.await

        Task.succeed {}

    result <- Task.attempt task

    when result is
        Ok {} -> Task.succeed mainType
        Err (FileWriteErr path NotFound) ->
            pathStr = Path.display path
            {} <- Stderr.line "One of the file writes got a NotFound error on path \(pathStr) while generating fuzz templates" |> Task.await

            Task.fail 1

        Err (FileWriteErr _ _) ->
            {} <- Stderr.line "One of the file writes errored while generating fuzz templates" |> Task.await

            Task.fail 1

Val : [
    Str Str,
    U8 U8,
    I8 I8,
    U16 U16,
    I16 I16,
    U32 U32,
    I32 I32,
    U64 U64,
    I64 I64,
    U128 U128,
    I128 I128,
    List (List Val),
]

valFromType : Type -> Val
valFromType = \type ->
    # TODO randomly generate these values
    when type is
        Str -> Str "a string"
        U8 -> U8 8
        I8 -> I8 -19
        U16 -> U16 1234
        I16 -> I16 -4321
        U32 -> U32 2344567
        I32 -> I32 -2345982
        U64 -> U64 2435082345203450
        I64 -> I64 -72435082345203450
        U128 -> U128 12811290341234109324103249123409132049
        I128 -> I128 -8128112903412341093241032491234091329
        ListStr -> List [Str "a string elem", Str "another string elem"]

valToRustVal : Val -> Str
valToRustVal = \val ->
    when val is
        Str str -> "roc_std::RocStr::from(\"\(str)\")"
        U8 num -> Num.toStr num
        I8 num -> Num.toStr num
        U16 num -> Num.toStr num
        I16 num -> Num.toStr num
        U32 num -> Num.toStr num
        I32 num -> Num.toStr num
        U64 num -> Num.toStr num
        I64 num -> Num.toStr num
        U128 num -> Num.toStr num
        I128 num -> Num.toStr num
        List list ->
            if List.isEmpty list then
                "roc_std::RocList::<RocStr>::empty()"
            else
                elemsStr = List.walk list "" \accum, elem ->
                    valStr = valToRustVal elem

                    if Str.isEmpty accum then
                        valStr
                    else
                        "\(accum), \(valStr)"

                "roc_std::RocList::from([\(elemsStr)])"

valToStr : Val -> Str
valToStr = \val -> valToStrHelp "" val

valToStrHelp : Str, Val -> Str
valToStrHelp = \buf, val ->
    when val is
        Str str -> Str.concat buf str
        U8 num -> Str.concat buf (Num.toStr num)
        I8 num -> Str.concat buf (Num.toStr num)
        U16 num -> Str.concat buf (Num.toStr num)
        I16 num -> Str.concat buf (Num.toStr num)
        U32 num -> Str.concat buf (Num.toStr num)
        I32 num -> Str.concat buf (Num.toStr num)
        U64 num -> Str.concat buf (Num.toStr num)
        I64 num -> Str.concat buf (Num.toStr num)
        U128 num -> Str.concat buf (Num.toStr num)
        I128 num -> Str.concat buf (Num.toStr num)
        List vals ->
            str = List.walk vals "" \accum, elem ->
                elemStr = valToStr elem

                if Str.isEmpty accum then
                    elemStr
                else
                    accum |> Str.concat ", \(elemStr)"

            Str.concat buf str

genHelp : List Val, Type -> { roc : Str, rust : Str, expected : Str }
genHelp = \vals, mainRetType ->
    init = {
        roc: "answer = \n        \"\"",
        rust: "",
        expected: "",
        index: 0,
    }

    answer =
        List.walk vals init \{ roc, rust, expected, index }, val ->
            indexStr = Num.toStr index
            rustStr = valToRustVal val
            nextRoc =
                when val is
                    Str _ -> "Str.concat arg\(indexStr)"
                    U8 _ | I8 _ | U16 _ | I16 _ | U32 _ | I32 _ | U64 _ | I64 _ | U128 _ | I128 _ -> "Str.concat (Num.toStr arg\(indexStr))"
                    List _ -> "Str.concat (Str.joinWith arg\(indexStr) \", \")"

            {
                expected: expected |> Str.concat (valToStr val),
                roc: "\(roc)\n        |> \(nextRoc)",
                rust: if Str.isEmpty rust then rustStr else "\(rust),\n        \(rustStr)",
                index: index + 1,
            }

    rocAnswer =
        when mainRetType is
            Str -> "answer"
            U8 -> "Num.toU8 (Str.countUtf8Bytes answer)"
            I8 -> "Num.toI8 (Str.countUtf8Bytes answer)"
            U16 -> "Num.toU16 (Str.countUtf8Bytes answer)"
            I16 -> "Num.toI16 (Str.countUtf8Bytes answer)"
            U32 -> "Num.toU32 (Str.countUtf8Bytes answer)"
            I32 -> "Num.toI32 (Str.countUtf8Bytes answer)"
            U64 -> "Num.toU64 (Str.countUtf8Bytes answer)"
            I64 -> "Num.toI64 (Str.countUtf8Bytes answer)"
            U128 -> "Num.toU128 (Str.countUtf8Bytes answer)"
            I128 -> "Num.toI128 (Str.countUtf8Bytes answer)"
            ListStr -> "[answer]"

    rustExpected =
        when mainRetType is
            Str -> "roc_std::RocStr::from(\"\(answer.expected)\")"
            U8 | I8 | U16 | I16 | U32 | I32 | U64 | I64 | U128 | I128 -> "answer"
            ListStr -> "roc_std::RocList::<roc_std::RocStr>::from([roc_std::RocStr::from(\"\(answer.expected)\")])"

    {
        roc: "\(answer.roc)\n\n    \(rocAnswer)",
        rust: answer.rust,
        expected: rustExpected,
    }
