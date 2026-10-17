app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

# Unused top-level values still own their compile-time observations.
unused = {
    expect 1 == 2
    42
}

main! = |_args| {
    Stdout.line!("runtime entrypoint")
    Ok({})
}
