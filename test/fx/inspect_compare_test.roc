app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Type WITH to_inspect
ColorWithInspect := [Red, Green, Blue].{
    to_inspect : ColorWithInspect -> Str
    to_inspect = |color| match color {
        Red => "Custom::Red"
        Green => "Custom::Green"
        Blue => "Custom::Blue"
    }
}

# Type WITHOUT to_inspect
ColorWithoutInspect := [Red, Green, Blue]

main! = || {
    # Test with to_inspect
    c1 : ColorWithInspect
    c1 = Red
    result1 = Str.inspect(c1)
    Stdout.line!("With to_inspect: ${result1}")

    # Test without to_inspect
    c2 : ColorWithoutInspect
    c2 = Red
    result2 = Str.inspect(c2)
    Stdout.line!("Without to_inspect: ${result2}")

    # Test primitive
    result3 = Str.inspect(42)
    Stdout.line!("Primitive: ${result3}")
}
