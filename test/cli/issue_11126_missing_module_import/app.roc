# repro for https://github.com/roc-lang/roc/issues/11126
# `Missing.roc` does not exist, so this app only ever reaches a "file not
# found" report: `roc run` must print that report and exit non-zero rather
# than crashing the compiler afterwards.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Missing

main! = || {}
