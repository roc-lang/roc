app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout

broken : U64
broken = "wrong type"

main! = || Stdout.line!("entrypoint ran")
