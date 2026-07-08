app [main!] {
    pf: platform "../../fx/platform/main.roc",
    repro: "../issue_9879_imported_tag_pkg/main.roc",
}

import repro.Repro
import repro.Style

# Repro for https://github.com/roc-lang/roc/issues/9879:
# checking should succeed when a package function accepts List of an imported tag union.
main! = || {
    _ = Repro.style([Style.A])
    {}
}
