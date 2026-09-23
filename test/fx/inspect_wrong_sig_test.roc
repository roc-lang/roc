app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# This to_inspect returns I64, so it is not an inspect override and
# Str.inspect renders the default form.
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
    result = Str.inspect(red)
    Stdout.line!("Result: ${result}")
}
