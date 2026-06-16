platform ""
	requires {} { main! : () => {} }
	exposes [Reactive, Elem, NodeValue]
	packages {}
	provides {
		"roc_main": main_for_host!,
		"roc_call_transform": call_transform,
		"roc_call_step": call_step,
		"roc_call_predicate": call_predicate,
	}
	hosted {
		"roc_host_append_child": Host.append_child!,
		"roc_host_bind_check": Host.bind_check!,
		"roc_host_bind_click": Host.bind_click!,
		"roc_host_bind_checked": Host.bind_checked!,
		"roc_host_bind_disabled": Host.bind_disabled!,
		"roc_host_bind_input": Host.bind_input!,
		"roc_host_bind_text": Host.bind_text!,
		"roc_host_bind_value": Host.bind_value!,
		"roc_host_create_dynamic": Elem.create_dynamic!,
		"roc_host_create_dynamic_keyed": Elem.create_dynamic_keyed!,
		"roc_host_create_element": Host.create_element!,
		"roc_host_create_each": Elem.create_each!,
		"roc_host_create_event_filter": Host.create_event_filter!,
		"roc_host_create_event_map": Host.create_event_map!,
		"roc_host_create_event_map_unit_i64_const": Host.create_event_map_unit_i64_const!,
		"roc_host_create_event_merge": Host.create_event_merge!,
		"roc_host_create_event_source": Host.create_event_source!,
		"roc_host_create_event_with_latest": Host.create_event_with_latest!,
		"roc_host_create_root": Host.create_root!,
		"roc_host_create_signal_const_bool": Host.create_signal_const_bool!,
		"roc_host_create_signal_const": Host.create_signal_const!,
		"roc_host_create_signal_const_i64": Host.create_signal_const_i64!,
		"roc_host_create_signal_const_str": Host.create_signal_const_str!,
		"roc_host_create_signal_fold_bool_toggle": Host.create_signal_fold_bool_toggle!,
		"roc_host_create_signal_fold": Host.create_signal_fold!,
		"roc_host_create_signal_fold_i64": Host.create_signal_fold_i64!,
		"roc_host_create_signal_hold": Host.create_signal_hold!,
		"roc_host_create_signal_map": Host.create_signal_map!,
		"roc_host_create_signal_map_i64_i64": Host.create_signal_map_i64_i64!,
		"roc_host_create_signal_map_i64_str": Host.create_signal_map_i64_str!,
		"roc_host_create_signal_map2": Host.create_signal_map2!,
		"roc_host_create_signal_map2_i64_i64": Host.create_signal_map2_i64_i64!,
		"roc_host_create_signal_map2_i64_i64_str": Host.create_signal_map2_i64_i64_str!,
		"roc_host_create_signal_state": Host.create_signal_state!,
		"roc_host_create_signal_zip_with": Host.create_signal_zip_with!,
		"roc_host_register_lifecycle": Elem.register_lifecycle!,
		"roc_host_send_event": Host.send_event!,
		"roc_host_set_checked": Host.set_checked!,
		"roc_host_set_disabled": Host.set_disabled!,
		"roc_host_set_label": Host.set_label!,
		"roc_host_set_role": Host.set_role!,
		"roc_host_set_test_id": Host.set_test_id!,
		"roc_host_set_text": Host.set_text!,
		"roc_host_set_value": Host.set_value!,
		"roc_host_bind_signal_update": Host.bind_signal_update!,
	}
	targets: {
		inputs: "targets/",
		x64mac: { inputs: ["libhost.a", app] },
		arm64mac: { inputs: ["libhost.a", app] },
	}

import Elem exposing [Elem]
import Reactive
import NodeValue exposing [NodeValue]
import Graph
import Host

## Called by host at startup to build the UI
main_for_host! : () => {}
main_for_host! = || main!()

## Called by host to evaluate a transform closure (for map operations)
call_transform : Box((NodeValue -> NodeValue)), NodeValue -> NodeValue
call_transform = |boxed_fn, input| {
	fn = Box.unbox(boxed_fn)
	fn(input)
}

## Called by host to evaluate a step closure (for fold, zip_with)
call_step : Box(((NodeValue, NodeValue) -> NodeValue)), NodeValue, NodeValue -> NodeValue
call_step = |boxed_step, acc, event_val| {
	step = Box.unbox(boxed_step)
	step((acc, event_val))
}

## Called by host to evaluate a predicate closure (for filter)
call_predicate : Box((NodeValue -> Bool)), NodeValue -> Bool
call_predicate = |boxed_pred, input| {
	pred = Box.unbox(boxed_pred)
	pred(input)
}
