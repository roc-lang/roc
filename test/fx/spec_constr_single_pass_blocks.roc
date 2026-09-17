app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Strict block prefixes must survive specialization exactly once, including
# when a later closure-producing branch moves the shared continuation.
scenario! : Str => I64
scenario! = |mode| {
    base = {
        Stdout.line!("outer")
        inner = {
            Stdout.line!("inner")
            if mode == "right" 19 else 9
        }
        Stdout.line!("after-inner")
        inner + 1
    }
    add_captured = |remaining, value| {
        if remaining == 0 value else add_captured(remaining - 1, value + base)
    }
    expect mode != ""
    Stdout.line!("prefix")
    if mode == "stop" {
        Stdout.line!("early")
        return 99
    }
    transform = if mode == "left" {
        Stdout.line!("left")
        |value| value + base
    } else {
        Stdout.line!("right")
        |value| value * base
    }
    Stdout.line!("shared")
    add_captured(2, transform(2))
}

main! = || {
    first = scenario!(Stdin.line!())
    Stdout.line!("result: ${first.to_str()}")
    second = scenario!(Stdin.line!())
    Stdout.line!("result: ${second.to_str()}")
    third = scenario!(Stdin.line!())
    Stdout.line!("result: ${third.to_str()}")
}
