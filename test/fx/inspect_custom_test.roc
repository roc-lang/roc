app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Define an opaque type with a custom to_inspect method
Color := [Red, Green, Blue].{
    to_inspect : Color -> Str
    to_inspect = |color| match color {
        Red => "Color::Red"
        Green => "Color::Green"
        Blue => "Color::Blue"
    }
}

main! = || {
    red : Color
    red = Red

    # Test Str.inspekt with custom to_inspect method
    result = Str.inspekt(red)
    Stdout.line!(result)

    # Compare with what the default would be
    Stdout.line!("Expected: Color::Red")
}
