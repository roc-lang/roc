#
# Shows how Roc values can be logged
#
app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect
import Community

main! = ||
    Community.empty
    |> Community.add_person({
        first_name: "John",
        last_name: "Smith",
        age: 27,
        has_beard: Bool.true,
        favorite_color: Blue,
    })
    |> Community.add_person({
        first_name: "Debby",
        last_name: "Johnson",
        age: 47,
        has_beard: Bool.false,
        favorite_color: Green,
    })
    |> Community.add_person({
        first_name: "Jane",
        last_name: "Doe",
        age: 33,
        has_beard: Bool.false,
        favorite_color: RGB((255, 255, 0)),
    })
    |> Community.add_friend(0, 2)
    |> Community.add_friend(1, 2)
    |> Inspect.to_str
    |> Effect.put_line!
