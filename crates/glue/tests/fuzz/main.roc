app "fuzz-glue"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Arg,
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
expectedPath = "generated/expected.txt"

# TODO just cp all this stuff over
genCargoTomlPath = "generated/Cargo.toml"
genHostCPath = "generated/host.c"
genBuildRsPath = "generated/build.rs"

main : Task {} U32
main =
    args <- Arg.list |> Task.await

    mainRetType =
        when List.get args 1 is
            Ok "Str" -> "Str"
            Ok "List Str" -> "List Str"
            _ -> crash "unrecognized fuzzer args"

    gen [] mainRetType


gen : List Str, Str -> Task {} U32
gen = \mainArgTypes, mainRetType ->
    # TODO rm -rf generated/
    # TODO mkdir -p generated/src/

    mainArgVals = List.map mainArgTypes genVal # TODO randomly generate

    # Actually use all the arguments to determine the return value,
    # so we have some way to tell if they made it through correctly.
    { roc: mainBody, rust, expected, mainForHostExpected } = combineArgs mainArgVals mainRetType

    mainType =
        if List.isEmpty mainArgTypes then
            mainRetType
        else
            mainArgTypes
            |> Str.joinWith ", "
            |> Str.concat " -> \(mainRetType)"

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
        |> Result.withDefault "{{ mainType }} not found"
        |> Str.replaceFirst "# {{ mainBody }}" mainBody
        |> Result.withDefault "{{ mainBody }} not found"

    genPlatContent =
        platformTemplate
        |> Str.replaceFirst "# {{ mainType }}" mainType
        |> Result.withDefault "{{ mainType }} not found"
        |> Str.replaceFirst "# {{ mainForHostType }}" mainType
        |> Result.withDefault "{{ mainForHostType }} not found"
        |> Str.replaceFirst "# {{ mainForHostBody }}" mainForHostBody
        |> Result.withDefault "{{ mainForHostBody }} not found"

    genHostContent =
        hostTemplate
        |> Str.replaceFirst "// {{ mainForHostArgs }}" rust
        |> Result.withDefault "{{ mainForHostArgs }} not found"
        |> Str.replaceFirst "// {{ mainForHostExpected }}" mainForHostExpected
        |> Result.withDefault "{{ mainForHostExpected }} not found"

    task =
        {} <- File.writeUtf8 (Path.fromStr genAppPath) genAppContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genPlatPath) genPlatContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr expectedPath) expected |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostLibPath) genHostContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostCPath) hostC |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genBuildRsPath) buildRs |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genCargoTomlPath) cargoToml |> Task.await

        Stdout.line "Generated \(genAppPath) and \(genPlatPath) and \(expectedPath)"

    result <- Task.attempt task

    when result is
        Ok {} -> Task.succeed {}
        Err (FileWriteErr path NotFound) ->
            pathStr = Path.display path
            {} <- Stderr.line "One of the file writes got a NotFound error on path \(pathStr) while generating fuzz templates" |> Task.await

            Task.fail 1
        Err (FileWriteErr _ _) ->
            {} <- Stderr.line "One of the file writes errored while generating fuzz templates" |> Task.await

            Task.fail 1

Val : [
    Str Str
]

genVal : Str -> Val
genVal = \type ->
    when type is
        "Str" -> Str "\"a string\""
        _ -> crash "Unrecognized type string: \"\(type)\""

combineArgs : List Val, Str -> { roc : Str, rust : Str, expected : Str, mainForHostExpected : Str }
combineArgs = \vals, mainRetType ->
    (mainRocVal, rustExpected) =
        when mainRetType is
            "Str" -> ("\"\"", "roc_std::RocStr::empty()")
            "List Str" -> ("[]", "roc_std::RocList::<roc_std::RocStr>::empty()")
            _ -> crash "unsupported mainRetType \(mainRetType)"

    init = {
        roc: "answer = \n         \(mainRocVal)",
        rust: "",
        expected: "",
        index: 0,
        mainForHostExpected: rustExpected,
    }

    answer =
        List.walk vals init \{ roc, rust, expected, index, mainForHostExpected }, val ->
            indexStr = Num.toStr index
            next =
                when val is
                    Str str ->
                        {
                            roc: "Str.concat arg\(indexStr)",
                            rust: "\"\(str)\"",
                            expected: expected |> Str.concat str,
                        }

            {
                expected: next.expected,
                roc: "\(roc)\n    |> \(next.roc)",
                rust: if Str.isEmpty rust then next.rust else ", \(next.rust)",
                index: index + 1,
                mainForHostExpected,
            }

    {
        roc: "\(answer.roc)\n\n    answer",
        rust: answer.rust,
        expected: "\(answer.expected)\n\n    answer",
        mainForHostExpected: answer.mainForHostExpected,
     }
