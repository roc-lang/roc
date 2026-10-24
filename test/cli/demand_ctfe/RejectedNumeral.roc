app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

Rejected := { value : U64 }.{
    from_numeral : Numeral -> Try(Rejected, [InvalidNumeral(Str)])
    from_numeral = |_| Err(InvalidNumeral("demanded runtime numeral"))

    plus : Rejected, Rejected -> Rejected
    plus = |a, b| { value: a.value + b.value }
}

# Only the runtime entrypoint instantiates the literal's target type.
add_one = |x| x.plus(1)

main! = |args| {
    input : Rejected
    input = { value: args.len().to_u64() }
    output = add_one(input)
    Stdout.line!(output.value.to_str())
    Ok({})
}
