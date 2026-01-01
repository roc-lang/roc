app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = parse_value()
    Stdout.line!("parsed: ${Str.inspect(result)}")
}

TokenContents : [
    IdentToken(Str),
]

Value := [
    UInt(U64),
]

parse_value : () -> Try((Try(TokenContents, Str), Value), Str)
parse_value = || {
    Ok((Ok(IdentToken("todo")), UInt(3)))
}
