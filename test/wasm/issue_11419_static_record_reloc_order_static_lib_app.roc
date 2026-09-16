app [main!] { pf: platform "./static-lib-platform/main.roc" }

# `kind` is 8-byte aligned (Gauge's I64s), so it is laid out before `id`
# (a List, 4-byte aligned on wasm32), while `id` comes first by field name.
# A constant `Node`'s two pointers are therefore written in the opposite order
# to the fields that carry them, and wasm-ld refuses an object whose
# relocations are not in offset order.
Kind : [Label(List(U8)), Gauge(I64, I64)]

Node : { id : List(U8), kind : Kind }

label : List(U8), List(U8) -> Node
label = |id, txt| { id, kind: Label(txt) }

nodes : List(Node)
nodes = [label([105, 100], [116, 120, 116]), label([105, 100, 50], [116, 120, 116, 50])]

# The index comes from the host, so `nodes` stays data: every pointer in it is
# a relocation in the object.
main! = |seed| {
    match List.get(nodes, seed) {
        Ok(node) =>
            match node.kind {
                Label(txt) => "${Str.from_utf8_lossy(node.id)}=${Str.from_utf8_lossy(txt)}"
                Gauge(_, _) => "gauge"
            }
        Err(_) => "none"
    }
}
