app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Type with to_inspect returning wrong type (I64 instead of Str)
BadColor := [Red, Green, Blue].{
    to_inspect : BadColor -> I64
    to_inspect = |color| match color {
        Red => 1
        Green => 2
        Blue => 3
    }
}

main! = || {
    red : BadColor
    red = Red
    result = Str.inspekt(red)
    Stdout.line!("Result: ${result}")
}
