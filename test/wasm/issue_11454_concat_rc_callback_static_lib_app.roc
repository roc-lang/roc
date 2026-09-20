app [main!] { pf: platform "./platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/11454
#
# Two `List.concat` calls over different refcounted element types keep the
# element incref/decref callbacks that `roc_builtins_list_concat` receives
# indirect, so wasm type-checks them at the `call_indirect`. The hosted call
# keeps the body out of compile-time evaluation, and the result joins all four
# host-derived strings.

import pf.FallibleHost

main! : () => Str
main! = || {
    input = FallibleHost.json_input!({})

    strs = List.concat([Str.concat(input, " a")], [Str.concat(input, " b")])
    lists = List.concat([[Str.concat(input, " c")]], [[Str.concat(input, " d")]])

    Str.join_with(List.concat(strs, List.join(lists)), ", ")
}
