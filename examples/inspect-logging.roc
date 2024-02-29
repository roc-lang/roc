#
# Shows how Roc values can be logged
#
app "inspect-logging"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    provides [main] to pf

import pf.Stdout
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
    |> Stdout.line
