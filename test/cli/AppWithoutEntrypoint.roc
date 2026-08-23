# `roc test` runs a file's `expect`s and nothing else, so this app is testable
# even though it never defines the `main!` its platform requires. `roc check`
# still reports the missing entrypoint.
app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Rb

expect Rb.run(Rb.field(41)) + 1 == 42
