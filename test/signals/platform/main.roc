platform ""
	requires {
		main : {} -> Elem
	}
	exposes [Elem, Signal, Html, Ui]
	packages {}
	provides { "roc_ui_init": ui_init }
	hosted {
		"roc_host_value_clone": HostValue.clone,
		"roc_host_value_get": HostValue.get,
		"roc_host_value_get_tagged": HostValue.get_tagged,
		"roc_host_value_store": HostValue.store,
		"roc_host_value_store_tagged": HostValue.store_tagged,
		"roc_host_value_take": HostValue.take,
		"roc_host_value_take_tagged": HostValue.take_tagged,
	}
	targets: {
		inputs: "../targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
		wasm32: { inputs: ["host.wasm", app], output: Shared },
	}

import Elem exposing [Elem]
import HostValue
import Signal
import Html
import Ui

ui_init : {} -> Box(Elem)
ui_init = |_| {
	Box.box(main({}))
}
