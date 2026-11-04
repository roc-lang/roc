app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

unused = |_| Json.to_str_try({ n: 1.U64 })

main! = || Stdout.line!("done")
