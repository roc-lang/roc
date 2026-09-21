app [main!] { pf: platform "./platform/main.roc" }

# A closure capturing a refcounted value, boxed as an erased callable: its
# final-drop slot is the published `Payload.on_drop` ABI, so the generated
# helper stored there must be the host-shaped adapter. Wasm type-checks that
# signature at the `call_indirect` the erased-callable runtime makes. The
# hosted call keeps the body out of compile-time evaluation.

import pf.FallibleHost

main! : () => Str
main! = || {
    input = FallibleHost.json_input!({})
    boxed = Box.box(|suffix| Str.concat(input, suffix))
    Box.unbox(boxed)(" ok")
}
