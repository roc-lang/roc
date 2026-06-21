platform ""
	requires {} { main : {} -> Elem }
	exposes [Elem, NodeValue, Node, Signal, Html, Ui]
	packages {}
	provides {
		"roc_main": ui_init,
	}
	targets: {
		inputs_dir: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
		x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
		x64win: { inputs: ["host.lib", app] },
		arm64win: { inputs: ["host.lib", app] },
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
