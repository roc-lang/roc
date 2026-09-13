platform ""
	requires {} { main : {} -> Elem }
	exposes [Elem, NodeValue, Node, Signal, Ui]
	packages {}
	provides {
		"roc_main": ui_init,
	}
	targets: {
		inputs_dir: "targets/",
		x64musl: { inputs: [app], output: Archive },
		wasm32: { inputs: [app], output: Archive },
	}

import NodeValue exposing [NodeValue]
import Elem exposing [Elem]
import Node
import Signal
import Ui

ui_init : {} -> Box(Elem)
ui_init = |_| Box.box(main({}))
