app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

## Test nested tag matching in statement-position (generateMatchStmt path).
## The match is the last expression in main!, returning {}.

read_something! : {} => Try(Str, [NotFound, ..])
read_something! = |{}| Err(NotFound)

do_init! : {} => Try(Str, [Exit(I64), ..])
do_init! = |{}| {
    result = read_something!({})?
    Ok(result)
}

main! = || {
    match do_init!({}) {
        Ok(s) => Stdout.line!("ok: ${s}")
        Err(Exit(code)) => Stdout.line!("FAIL: got Exit(${Str.inspect(code)})")
        Err(_) => Stdout.line!("PASS: statement-position nested tag match works")
    }
}
