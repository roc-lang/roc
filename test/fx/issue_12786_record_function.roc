app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

persist : { id : U64 } -> U64
persist = |r| r.id

create = |inputs| (inputs.persist)({ id: inputs.n })

main! = || Stdout.line!(create({ n: 1.U64, persist: persist }).to_str())
