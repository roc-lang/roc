app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Define an opaque type WITHOUT a to_inspect method
Color := [Red, Green, Blue].{
    as_str : Color -> Str
    as_str = |color| match color {
        Red => "Red"
        Green => "Green"
        Blue => "Blue"
    }
}

main! = || {
    red : Color
    red = Red

    # Test Str.inspect without custom to_inspect method - should use default rendering
    result = Str.inspect(red)
    Stdout.line!("Result: ${result}")

    # Default would show the underlying tag
    Stdout.line!("(Default rendering)")
}
