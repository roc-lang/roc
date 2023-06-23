app "inspect-gui"
    packages { pf: "gui/platform/main.roc" }
    imports [
        Inspect,
        Community,
        GuiFormatter,
    ]
    provides [render] to pf

render =
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
    |> GuiFormatter.toGui
