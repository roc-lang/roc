platform ""
	requires {} { main : {} -> Elem }
	exposes [Reactive, Elem, NodeValue, Signal, Html, Ui]
	packages {}
	provides {
		"roc_ui_init": ui_init,
		"roc_ui_recompute": ui_recompute,
		"roc_ui_render": ui_render,
		"roc_ui_drop": ui_drop,
		"roc_ui_node_abi_probe": ui_node_abi_probe,
	}
	targets: {
		inputs: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
	}

import Elem exposing [Elem]
import Reactive
import NodeValue exposing [NodeValue]
import Node
import Signal
import Html
import Ui
import UiRuntime

ui_init : {} -> Box(UiRuntime.DispatchResult)
ui_init = |_| {
	result = UiRuntime.init(main({}))
	Box.box(result)
}

ui_recompute : Box(UiRuntime.Runtime), Box(UiRuntime.RecomputeInput) -> Box(UiRuntime.RecomputeResult)
ui_recompute = |runtime, input| {
	Box.box(UiRuntime.recompute(runtime, Box.unbox(input)))
}

ui_render : Box(UiRuntime.Runtime), Box(UiRuntime.RenderInput) -> Box(UiRuntime.DispatchResult)
ui_render = |runtime, input| {
	Box.box(UiRuntime.render_only(runtime, Box.unbox(input)))
}

ui_drop : Box(UiRuntime.Runtime) -> {}
ui_drop = |runtime| UiRuntime.drop(runtime)

ui_node_abi_probe : {} -> Node.Elem
ui_node_abi_probe = |_| Node.Elem.Element({ tag: "div", attrs: [], children: [] })
