app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

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

    Stdout.line!("My favourite color is ${red.as_str()}")
}
