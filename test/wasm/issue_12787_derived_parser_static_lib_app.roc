app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

Inner : { favoritesCount : U64 }

Model : { wrapped : Inner, tag : [A(U64), B(Str)] }

Wide : [InvalidJson(Str), Rejected(Str)]

parse : Str -> Try(Model, Wide)
parse = |json| {
    if json == "rejected" {
        Err(Rejected("rejected before parsing: long enough for the heap"))
    } else {
        Json.parse(json)
    }
}

show : Try(Model, Wide) -> Str
show = |result| {
    match result {
        Ok({ wrapped, tag: A(number) }) => "ok ${wrapped.favoritesCount.to_str()} A ${number.to_str()}"
        Ok({ wrapped, tag: B(text) }) => "ok ${wrapped.favoritesCount.to_str()} B ${text}"
        Err(InvalidJson(_)) => "invalid"
        Err(Rejected(reason)) => reason
    }
}

main! : () => Str
main! = || {
    input = FallibleHost.json_input!({})
    a_case = Str.concat(Str.concat("{\"wrapped\":", input), ",\"tag\":{\"A\":5}}")
    b_case = Str.concat(Str.concat("{\"tag\":{\"B\":\"text\"},\"wrapped\":", input), "}")
    missing = Str.concat(Str.concat("{\"wrapped\":", input), "}")
    invalid = Str.concat("not json ", input)
    rejected = if input == "" { "" } else { "rejected" }
    "${show(parse(a_case))}|${show(parse(b_case))}|${show(parse(missing))}|${show(parse(invalid))}|${show(parse(rejected))}"
}
