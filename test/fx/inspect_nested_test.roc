app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Define a nominal type with a custom to_inspect method
Color := [Red, Green, Blue].{
    to_inspect : Color -> Str
    to_inspect = |color| match color {
        Red => "Color::Red"
        Green => "Color::Green"
        Blue => "Color::Blue"
    }
}

main! = || {
    # Create a Color value by annotating the variable type
    my_color : Color
    my_color = Red

    # Create a record containing the nominal type
    my_record = { name: "test", color: my_color, count: 42 }

    # Inspect the outer record - the Color field should use its custom to_inspect
    result = inspect my_record
    Stdout.line!(result)

    # Expected output should include "Color::Red" for the color field
    Stdout.line!("Expected: { color: Color::Red, count: 42, name: \"test\" }")
}
