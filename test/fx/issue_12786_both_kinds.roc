app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

required : { id : U64 } -> U64
required = |r| r.id

optional : { id ?: U64 } -> U64
optional = |r|
    match r.?id {
        Ok(id) => id + 1
        Err(MissingField) => 0
    }

create = |inputs| (inputs.persist)({ id: inputs.n })

main! = || {
    Stdout.line!(create({ n: 1.U64, persist: required }).to_str())
    Stdout.line!(create({ n: 1.U64, persist: optional }).to_str())
}
