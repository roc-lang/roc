app [main!] { pf: platform "../../fx/platform/main.roc" }

import RejectedBranches
import pf.Stdout

# This checked root fails by reading the failed literal and owns its report.
value : RejectedBranches.Quoted(I32)
value = RejectedBranches.get(1)

main! = || Stdout.line!(value.text)
