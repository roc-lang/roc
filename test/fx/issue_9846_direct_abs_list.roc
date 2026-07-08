app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

Model : { count : U64 }

render! : Model, Host.Host => Try(Model, [Exit(I64), ..])
render! = |_model, host| {
    runtime = host.get_greeting!()
    runtime_len = Str.count_utf8_bytes(runtime)

    input : F32
    input = if runtime_len > 0 { -1.0 } else { 1.0 }

    abs_value = F32.abs(input)
    values = if abs_value >= 0 and runtime_len > 0 [1] else []

    next_model = {
        count: match List.first(values) {
            Ok(_) => 1
            Err(_) => 0
        },
    }

    if runtime_len > 0 { Ok(next_model) } else { Err(Exit(1)) }
}

main! = || {
    match render!({ count: 0 }, Host.new("issue-9846")) {
        Ok(model) => Stdout.line!("count ${model.count.to_str()}")
        Err(_) => Stdout.line!("unexpected error")
    }
}
