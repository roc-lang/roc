app [main!] { pf: platform "./platform/main.roc" }

import pf.App

AppErrors : [ReadFailed, ParseFailed]

read_config : {} -> Try(Str, [ReadFailed, ..err])
read_config = |_| Err(ReadFailed)

parse_config : Str -> Try({}, [ParseFailed, ..err])
parse_config = |_| Err(ParseFailed)

main! : App.Main(AppErrors)
main! = |_args| {
    config = read_config({})?
    _ = parse_config(config)?
    Ok({})
}
