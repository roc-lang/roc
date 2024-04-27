#
# Shows how Roc values can be logged
#
app "inspect-logging"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.1/y_Ww7a2_ZGjp0ZTt9Y_pNdSqqMRdMLzHMKfdN8LWidk.tar.br" }
    imports [
        pf.Stdout,
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
    |> Inspect.toStr
    |> Stdout.line
