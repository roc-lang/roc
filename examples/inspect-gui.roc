app "inspect-gui"
    packages { pf: "gui/platform/main.roc" }
    imports [
        Inspect,
        Person,
        GuiFormatter,
    ]
    provides [render] to pf

render =
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

    Inspect.inspect person |> GuiFormatter.toGui
