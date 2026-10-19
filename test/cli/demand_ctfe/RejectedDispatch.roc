app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

Missing := { value : U64 }

describe : a -> Str where [a.describe : a -> Str]
describe = |value| value.describe()

main! = |args| {
    input : Missing
    input = { value: args.len().to_u64() }
    Stdout.line!(describe(input))
    Ok({})
}
