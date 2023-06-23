app "inspect-logging"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br" }
    imports [
        pf.Stdout,
        Inspect.{ Formatter, Inspector, Inspect },
        LogFormatter,
        Community,
    ]
    provides [main] to pf

main =
    Community.empty
    |> Community.addPerson {
        firstName: "John",
        lastName: "Smith",
        age: 27,
        hasBeard: Bool.true,
        favoriteColor: Blue,
    }
    |> Community.addPerson {
        firstName: "Debby",
        lastName: "Johnson",
        age: 47,
        hasBeard: Bool.false,
        favoriteColor: Green,
    }
    |> Community.addPerson {
        firstName: "Jane",
        lastName: "Doe",
        age: 33,
        hasBeard: Bool.false,
        favoriteColor: RGB (255, 255, 0),
    }
    |> Community.addFriend 0 2
    |> Community.addFriend 1 2
    |> Inspect.inspect
    |> LogFormatter.toBytes
    |> Str.fromUtf8
    |> unwrapOrCrash
    |> Stdout.line

unwrapOrCrash : Result ok err -> ok
unwrapOrCrash = \res ->
    when res is
        Ok v -> v
        Err _ -> crash "Hit an error in a result"
