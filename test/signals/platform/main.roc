platform ""
	requires {} { main : {} -> Elem }
	exposes [Elem, NodeValue, Node, Signal, Html, Ui]
	packages {}
	provides {
		"roc_ui_init": ui_init,
	}
	targets: {
		inputs: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
	}

import NodeValue exposing [NodeValue]
import Elem exposing [Elem]
import Node
import Signal
import Html
import Ui

ui_init : {} -> Box(Elem)
ui_init = |_| {
	Box.box(main({}))
}
