app [main!] { pf: platform "./platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/11455
#
# A nominal tag union whose only variant carries `Box({})`, a box of a
# zero-sized payload, appearing as the payload of a multi-variant tag union
# that is matched at a runtime-unknown value. `--opt=dev --target=wasm32` must
# emit a module that validates and runs; the `Element` arm returns the label
# the host handed us.

import pf.FallibleHost

Attribute(msg) := [OnVisible(Box(msg))]

main! : () => Str
main! = || {
    input = FallibleHost.json_input!({})
    match render(input) {
        Text(s) => s
        Element(_, label) => label
    }
}

render : Str -> [Text(Str), Element(Attribute({}), Str)]
render = |label| Element(OnVisible(Box.box({})), label)
