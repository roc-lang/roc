app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

# Checking reports a failed expect in a value nothing reads.
unused = {
    expect 1 == 2
    42
}

main! = |_args| {
    Stdout.line!("runtime entrypoint")
    Ok({})
}
