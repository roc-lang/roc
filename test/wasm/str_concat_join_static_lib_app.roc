app [main!] { pf: platform "./static-lib-platform/main.roc" }

choose : Bool, Str, Str -> Str
choose = |flag, yes, no|
    if flag {
        yes
    } else {
        no
    }

left_label : Bool -> Str
left_label = |flag| choose(flag, "X", "Y")

right_label : Bool -> Str
right_label = |flag| choose(flag, "1", "2")

short_line : Bool -> Str
short_line = |flag| {
    left = left_label(flag)
    right = right_label(flag)

    Str.concat(Str.concat(left, ":"), right)
}

main! = |_seed| Str.concat(short_line(True), short_line(False))
