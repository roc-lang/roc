# Uses `Echo.traced` at the platform constant's type and at a second type:
# each specialization reports its own `dbg`, once.
app [main!] { pf: platform "./dbg_platform/main.roc" }

import pf.Echo

main! = |args| {
    nums : List(U64)
    nums = List.append(Echo.traced, List.len(args))
    strs : List(Str)
    strs = List.concat(Echo.traced, args)
    Echo.line!(Str.inspect(List.len(nums) + List.len(strs)))
    Ok({})
}
