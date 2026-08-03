app [main] { pf: platform "platform/main.roc" }

import pf.Elem exposing [Elem]

main : {} -> Elem
main = |_| Elem.Text(decode_payload("49,50"))

decode_payload = |payload| {
    lines = payload.split_on("\n")
    body_line = read_line(lines)

    if !body_line.rest.is_empty() {
        crash "trailing fields"
    }

    body_line.value
}

read_line = |lines|
    match lines.first() {
        Ok(value) => { value, rest: lines.drop_first(1) }
        Err(_) => {
            crash "missing line"
        }
    }
