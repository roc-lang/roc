app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout
import pf.Host

# Every shape an interpolated `Str` literal can take, checked against the same
# string built by `Str.concat`. The values come from the host at runtime, so
# nothing folds at compile time.
check! : Str, Str, Str => {}
check! = |name, literal, expected|
    if literal == expected {
        Stdout.line!(Str.concat("ok ", name))
    } else {
        Stdout.line!(Str.concat(Str.concat(Str.concat(Str.concat("MISMATCH ", name), ": "), literal), Str.concat(" != ", expected)))
    }

main! = || {
    # "Hello, parts!" from the host
    greeting = Host.get_greeting!(Host.new("parts"))
    n = Str.count_utf8_bytes(greeting)
    a = n.to_str()
    e = Str.repeat("x", n - n)
    big = Str.repeat(greeting, 3)
    utf8 = "héllo→🙂"

    check!("one value", "${a}", a)
    check!("leading segment", "n=${a}", Str.concat("n=", a))
    check!("trailing segment", "${a}!", Str.concat(a, "!"))
    check!("both segments", "[${a}]", Str.concat(Str.concat("[", a), "]"))
    check!("empty value", "<${e}>", "<>")
    check!("three empties in a row", "${e}${e}${e}", "")
    check!("empty value between values", "${a}${e}${a}", Str.concat(a, a))
    check!("big value", "big:${big}", Str.concat("big:", big))
    check!("big values crossing both ways", "${big}|${a}|${big}", Str.concat(Str.concat(Str.concat(Str.concat(big, "|"), a), "|"), big))
    check!("utf8 segments", "é${a}→${utf8}🙂", Str.concat(Str.concat(Str.concat(Str.concat("é", a), "→"), utf8), "🙂"))
    check!("nested literal", "(${"${a}-${a}"})", Str.concat(Str.concat("(", Str.concat(Str.concat(a, "-"), a)), ")"))
    check!("number through to_str", "${(n + 1).to_str()}", (n + 1).to_str())
    check!("seventeen values", "${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}${a}", Str.repeat(a, 17))
    check!("shared heap value used after", "${big}${big}", Str.concat(big, big))
    check!("value still usable after", Str.concat(big, "."), Str.concat(big, "."))
    check!("greeting", "${greeting} and ${greeting}", Str.concat(Str.concat(greeting, " and "), greeting))
    Stdout.line!(a)
}
