app "fuzz-glue"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        pf.Arg,
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

genType : Generator Str
genType =
    Random.uniform "Str" ["List Str"]

genArgsAndResult : Generator (List Str, Str)
genArgsAndResult =
    args, ret <- Random.map2 (Random.list genType 12) genType
    (args, ret)

main : Task {} U32
main =
    # We expect 1 CLI arg, namely the RNG seed
    cliArgs <- Arg.list |> Task.await

    seed =
        List.get cliArgs 1
        |> Result.try Str.toU64
        |> Result.withDefault defaultSeed

    seedStr = Num.toStr seed
    {} <- Stdout.line "Generating from seed \(seedStr)" |> Task.await

    # Randomly generate main's arguments and return type from the provided seed
    (mainArgs, mainRetType) =
        seed
        |> Random.seed
        |> Random.step genArgsAndResult
        |> .0

    gen mainArgs mainRetType


gen : List Str, Str -> Task {} U32
gen = \mainArgTypes, mainRetType ->
    # TODO rm -rf generated/
    # TODO mkdir -p generated/src/

    mainArgVals = List.map mainArgTypes genVal

    # Actually use all the arguments to determine the return value,
    # so we have some way to tell if they made it through correctly.
    { roc: mainBody, rust, expected } = genHelp mainArgVals mainRetType

    mainType =
        if List.isEmpty mainArgTypes then
            mainRetType
        else
            mainArgTypes
            |> Str.joinWith ", "
            |> Str.concat " -> \(mainRetType)"

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
        |> Result.withDefault "{{ mainType }} not found"
        |> Str.replaceFirst "# {{ mainBody }}" mainBody
        |> Result.withDefault "{{ mainBody }} not found"
        |> Str.replaceFirst "# {{ mainLambda }}" mainLambda
        |> Result.withDefault "{{ mainLambda }} not found"

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
        |> Str.replaceFirst "// {{ mainForHostExpected }}" expected
        |> Result.withDefault "{{ mainForHostExpected }} not found"

    task =
        {} <- File.writeUtf8 (Path.fromStr genAppPath) genAppContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genPlatPath) genPlatContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostLibPath) genHostContent |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genHostCPath) hostC |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genBuildRsPath) buildRs |> Task.await
        {} <- File.writeUtf8 (Path.fromStr genCargoTomlPath) cargoToml |> Task.await

        Stdout.line "Generated \(genAppPath) and \(genPlatPath)"

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
    Str Str,
    List (List Val),
]

genVal : Str -> Val
genVal = \type ->
    # TODO randomly generate these values
    when type is
        "Str" -> Str "a string"
        "List Str" -> List [Str "a string elem", Str "another string elem"]
        _ -> crash "Unrecognized val type string: \"\(type)\""

valToRustVal : Val -> Str
valToRustVal = \val ->
    when val is
        Str str -> "roc_std::RocStr::from(\"\(str)\")"
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
        Str str ->
            Str.concat buf str

        List vals ->
            str = List.walk vals "" \accum, elem ->
                elemStr = valToStr elem

                if Str.isEmpty accum then
                    elemStr
                else
                    accum |> Str.concat ", \(elemStr)"

            Str.concat buf str


genHelp : List Val, Str -> { roc : Str, rust : Str, expected : Str }
genHelp = \vals, mainRetType ->
    initialRocVal =
        when mainRetType is
            "Str" -> "\"\""
            "List Str" -> "[]"
            _ -> crash "unsupported mainRetType \(mainRetType)"

    init = {
        roc: "answer = \n        \(initialRocVal)",
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
                    List _ -> "Str.concat (Str.joinWith arg\(indexStr) \", \")"

            {
                expected: expected |> Str.concat (valToStr val),
                roc: "\(roc)\n        |> \(nextRoc)",
                rust: if Str.isEmpty rust then rustStr else "\(rust),\n        \(rustStr)",
                index: index + 1,
            }

    rustExpected =
        when mainRetType is
            "Str" -> "roc_std::RocStr::from(\"\(answer.expected)\")"
            "List Str" -> "roc_std::RocList::<roc_std::RocStr>::empty()" # TODO
            _ -> crash "unsupported mainRetType \(mainRetType)"

    {
        roc: "\(answer.roc)\n\n    answer",
        rust: answer.rust,
        expected: rustExpected,
     }
