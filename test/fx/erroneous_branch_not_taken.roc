app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Entry : { checked : U64 }

# The `True` branch has a checked type mismatch. Only taking that branch may
# crash; the annotated type of `pick` and its `False` branch stay usable.
pick : Bool -> Entry
pick = |b| if b {
    {}
} else {
    { checked: 2 }
}

main! = || {
    Stdout.line!("checked: ${pick(False).checked.to_str()}")
    Stdout.line!("checked: ${pick(True).checked.to_str()}")
}
