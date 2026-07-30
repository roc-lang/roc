app [main!] { pf: platform "./main.roc" }

import pf.CliHost

main! : List(Str) => Try({}, [Exit(I32)])
main! = |args| {
    first_arg = match args.get(0) {
        Ok(arg) => arg
        Err(_) => ""
    }
    input = CliHost.read!({})
    CliHost.log!("roc saw ${input} argc=${args.len().to_str()} first=${first_arg}")
    Ok({})
}
