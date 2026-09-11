# Forward the helper's checked requirements across independently interned
# method-name stores. Both native and Wasm code generation must succeed.
app [main] { pf: platform "./platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.NodeValue exposing [NodeValue]
import pf.Signal
import pf.Ui

render_rows : Signal.Signal(List(item)), (item -> k), (k, Signal.Signal(item) -> Elem) -> Elem
	where [
		item.decode : NodeValue, NodeValue -> (Try(item, [TypeMismatch]), NodeValue),
		k.encoder_for : NodeValue -> (k, NodeValue -> Try(NodeValue, [])),
		k.decode : NodeValue, NodeValue -> (Try(k, [TypeMismatch]), NodeValue),
		k.to_hash : k, Hasher -> Hasher,
		k.is_eq : k, k -> Bool,
	]
render_rows = |rows, key_of, row| Ui.each(rows, key_of, row)

labels : Signal.Signal(List(Str))
labels = Signal.const(["a", "b"])

main : {} -> Elem
main = |_| render_rows(labels, |label| label, |_key, _item| Elem.Text("row"))
