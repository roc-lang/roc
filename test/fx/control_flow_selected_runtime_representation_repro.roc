app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

text = |content| [Text(content)].iter()

sidebar = |show| {
    section = if show {
        text("view")
    } else {
        [].iter()
    }
    section
}

main! = || {
    _ = sidebar(True)
    Stdout.line!("ok")
}
