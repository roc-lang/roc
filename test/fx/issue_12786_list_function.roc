app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

persist : { id : U64 } -> U64
persist = |r| r.id

double : { id : U64 } -> U64
double = |r| r.id * 2

create = |fs, n| List.map(fs, |f| f({ id: n }))

main! = || Stdout.line!(Str.inspect(create([persist, double], 3.U64)))
