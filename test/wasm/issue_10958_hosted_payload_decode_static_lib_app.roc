app [main!] { pf: platform "./platform/main.roc" }

import pf.Task

field_at : List(Str), U64 -> Str
field_at = |parts, index|
    match parts.get(index) {
        Ok(value) => value
        Err(_) => ""
    }

main! : () => Str
main! = || {
    payload = Task.payload!({})
    parts = payload.split_on(",")
    depart = field_at(parts, 2)

    # `depart` is a seamless slice into `payload`. Converting it to bytes
    # creates another owner of that backing allocation, while the comparison
    # below keeps the string owner live too.
    bytes = depart.to_utf8()
    if bytes.len() > 0 and depart != "" {
        "ok"
    } else {
        "bad"
    }
}
