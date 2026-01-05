app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Recursive type
Statement := [
    FuncCall({
        name: Str,
        args: List(U64),
    }),
    ForLoop({
        identifiers: List(Str),
        block: List(Statement),  # Recursive!
    }),
    IfStatement({
      condition: U64,
      block: List(Statement),  # Recursive!
    }),
]

parse_block! : List(Statement) => Try((List(Statement), U64), Str)
parse_block! = |acc| {
    Stdout.line!("Parsing block")
    token = SomeTag
    match token {
        OpenBraceToken => {
            (block, _index) = parse_block!([])?
            parse_block!(List.append(acc, ForLoop({identifiers: [], block})))  # Using block here
        }
        _ => Err("wildcard")
    }
}

main! = || {
    Stdout.line!("converted to utf8")
    parsed = parse_block!([])
    Stdout.line!("parsed: ${Str.inspect(parsed)}")
}
