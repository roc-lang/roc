# Repro for https://github.com/roc-lang/roc/issues/10370
app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

Input : { label : Str }

emit! : Str => Try({}, [EmitFailed(Str)])
emit! = |msg| {
    Stdout.line!(msg)
    Ok({})
}

begin! : Input => Try(U64, [BeginFailed(Str)])
begin! = |input|
    match emit!(input.label) {
        Ok({}) => Ok(1)
        Err(_) => Err(BeginFailed("nope"))
    }

after_impl! = |msg| emit!(msg).map_err(|err| Wrapped(err))

handlers = { after!: after_impl! }

run! = |handlers_, input| {
    after! = handlers_.after!

    match begin!(input) {
        Ok(_) => {
            result = middle!({})
            _ = after!("2 after")
            result
        }

        Err(err) => Err(err)
    }
}

middle! = |{}| {
    Stdout.line!("1 middle")
    Ok(Done)
}

main! = |args| {
    input = { label: "0 begin ${args.len().to_str()}" }
    _ = run!(handlers, input)
    Ok({})
}
