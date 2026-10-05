app [main!] { pf: platform "../../fx/platform/main.roc" }

import RejectedBranches
import pf.Stdout

# This checked root succeeds. Rejection belongs to the specialized literal.
value : RejectedBranches.Quoted(I32)
value = RejectedBranches.get(0)

main! = || Stdout.line!(value.text)
