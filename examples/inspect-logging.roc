app "inspect-logging"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br" }
    imports [
        pf.Stdout,
        Inspect.{ Formatter, Inspector, Inspect },
        LogFormatter,
        Person,
    ]
    provides [main] to pf

main =
    person = Person.new {
        firstName: "John",
        lastName: "Smith",
        age: 27,
        hasBeard: Bool.true,
        data: Dict.fromList [
            ("test", Thing 7),
            ("stuff", Stuff),
        ],
    }

    Inspect.inspect person
    |> LogFormatter.toBytes
    |> Str.fromUtf8
    |> unwrapOrCrash
    |> Stdout.line

unwrapOrCrash : Result ok err -> ok
unwrapOrCrash = \res ->
    when res is
        Ok v -> v
        Err _ -> crash "Hit an error in a result"
