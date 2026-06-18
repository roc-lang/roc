platform ""
	requires {} { main : {} -> Elem }
	exposes [Reactive, Elem, NodeValue]
	packages {}
	provides {
		"roc_ui_init": ui_init,
		"roc_ui_recompute": ui_recompute,
		"roc_ui_render": ui_render,
		"roc_ui_dispatch": ui_dispatch,
		"roc_ui_drop": ui_drop,
	}
	targets: {
		inputs: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
	}

import Elem exposing [Elem]
import Reactive
import NodeValue exposing [NodeValue]
import UiRuntime

ui_init : {} -> Box(UiRuntime.DispatchResult)
ui_init = |_| {
	result = UiRuntime.init(main({}))
	Box.box(result)
}

ui_dispatch : Box(UiRuntime.Runtime), Box(UiRuntime.HostEvent) -> Box(UiRuntime.DispatchResult)
ui_dispatch = |runtime, event| {
	Box.box(UiRuntime.dispatch(runtime, Box.unbox(event)))
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
