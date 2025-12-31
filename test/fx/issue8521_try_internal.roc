app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Regression test for issue #8521: App/Platform return type not fully unified
#
# This test uses Try internally within the app and verifies
# that type inference works correctly for tag union returns.

get_greeting : {} -> Try(Str, [ListWasEmpty])
get_greeting = |{}| {
    first = List.first(["Testing issue 8521 with Try"])?
    Ok(first)
}

main! = || {
    match get_greeting({}) {
        Ok(msg) => Stdout.line!(msg)
        Err(ListWasEmpty) => Stdout.line!("Empty!")
    }
}
