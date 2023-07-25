app "fuzz-glue"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        "static/app-template.roc" as appTemplate : Str,
        "static/platform-template.roc" as platformTemplate : Str,
        "static/host-template.rs" as hostTemplate : Str,
    ]
    provides [main] to pf

generatedAppPath = "generated/app.roc"
generatedPlatformPath = "generated/platform.roc"
generatedHostPath = "generated/src/lib.rs"
expectedPath = "generated/expected.txt"

main : Task {} U32
main =
    mainArgTypes = [] # TODO generate
    mainRetType = "Str" # TODO generate

    # TODO rm -rf generated/
    # TODO mkdir -p generated/src/

    mainArgVals = List.map mainArgTypes genVal # TODO randomly generate
    mainRetVal = genVal mainRetType # TODO randomly generate

    # Actually use all the arguments to determine the return value,
    # so we have some way to tell if they made it through correctly.
    { roc: mainBody, rust, expected } = combineArgs mainArgVals

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

    task =
        {} <-
            content =
                appTemplate
                |> Str.replaceFirst "# {{ mainType }}" mainType
                |> Result.withDefault "{{ mainType }} not found"
                |> Str.replaceFirst "# {{ mainBody }}" mainBody
                |> Result.withDefault "{{ mainBody }} not found"

            File.writeUtf8 (Path.fromStr generatedAppPath) content |> Task.await

        {} <-
            content =
                platformTemplate
                |> Str.replaceFirst "# {{ mainForHostType }}" mainType
                |> Str.replaceFirst "# {{ mainForHostBody }}" mainForHostBody

            File.writeUtf8 (Path.fromStr generatedPlatformPath) content |> Task.await

        {} <-
            hostTemplate
            |> Str.replaceFirst "// {{ mainForHostArgs }}" rust
            |> File.writeUtf8 (Path.fromStr generatedHostPath)
            |> Task.await

        {} <-
            expected
            |> File.writeUtf8 (Path.fromStr expectedPath)
            |> Task.await

        Stdout.line "Generated \(generatedAppPath) and \(generatedPlatformPath) and \(expectedPath)"

    result <- Task.attempt

    when result is
        Ok {} -> Task.succeed {}
        Err (FileWriteErr _) ->
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

combineArgs : List Val -> { roc : Str, rust : Str, expected : Str }
combineArgs = \vals ->
    init = {
        roc: "answer = \n     \"\"",
        rust: "",
        expected: "",
        index: 0,
    }

    answer =
        List.walk vals init \{ roc, rust, expected, index }, val ->
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
            }

    {
        roc: answer.roc,
        rust: answer.rust,
        expected: "\(answer.expected)\n\n    answer"
     }
