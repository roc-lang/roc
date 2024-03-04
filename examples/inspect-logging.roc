#
# Shows how Roc values can be logged
#
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Stdout
import pf.Task
import Community

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
        |> Stdout.line!
