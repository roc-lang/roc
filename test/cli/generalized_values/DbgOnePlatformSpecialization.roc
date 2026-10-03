# Uses `Echo.traced` at the one type the platform's own constant also uses, so
# both finalization programs evaluate that specialization; its `dbg` is
# reported once.
app [main!] { pf: platform "./dbg_platform/main.roc" }

import pf.Echo

main! = |args| {
    nums : List(U64)
    nums = List.append(Echo.traced, List.len(args))
    Echo.line!(Str.inspect(nums))
    Ok({})
}
