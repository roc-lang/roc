## Hosted effects - implemented by host (Zig), called by Roc.
## The host stores all state including boxed closures.
##
## NodeId (U64) identifies a node in the host's reactive graph.
## ElemId (U64) identifies a DOM element.

import NodeValue exposing [NodeValue]

## Host effects for creating DOM elements and graph nodes
Host := [].{

	## Create the root element container
	create_root! : () => U64

	## Create a DOM element by tag name
	create_element! : Str => U64

	## Set element text content
	set_text! : U64, Str => {}

	## Set semantic/test metadata
	set_role! : U64, Str => {}

	set_label! : U64, Str => {}

	set_test_id! : U64, Str => {}

	## Set element form state
	set_value! : U64, Str => {}

	set_checked! : U64, Bool => {}

	set_disabled! : U64, Bool => {}

	## Append child to parent element
	append_child! : U64, U64 => {}

	## Create an event source node in the graph
	create_event_source! : () => U64

	## Create an event map node (host stores the transform closure)
	create_event_map! : U64, Box((NodeValue -> NodeValue)) => U64

	## Create a scalar event map node from Unit events to a fixed I64 value
	create_event_map_unit_i64_const! : U64, I64 => U64

	## Create an event filter node (host stores the predicate closure)
	create_event_filter! : U64, Box((NodeValue -> Bool)) => U64

	## Create an event merge node
	create_event_merge! : U64, U64 => U64

	## Create an event node that samples a signal when the event fires
	create_event_with_latest! : U64, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

	## Create a constant signal node
	create_signal_const! : NodeValue => U64

	## Create typed constant signal nodes
	create_signal_const_i64! : I64 => U64

	create_signal_const_bool! : Bool => U64

	create_signal_const_str! : Str => U64

	## Create a mutable root state signal node
	create_signal_state! : NodeValue => U64

	## Create a signal map node (host stores the transform closure)
	create_signal_map! : U64, Box((NodeValue -> NodeValue)) => U64

	## Create a signal map2 node (host stores the transform closure)
	create_signal_map2! : U64, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

	## Create a signal hold node
	create_signal_hold! : NodeValue, U64 => U64

	## Create a signal fold node (host stores the step closure)
	create_signal_fold! : NodeValue, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

	## Create typed scalar signal fold nodes
	create_signal_fold_i64! : I64, U64, Box(((I64, I64) -> I64)) => U64

	create_signal_fold_bool_toggle! : Bool, U64 => U64

	## Create a signal zip_with node (host stores the combine closure)
	create_signal_zip_with! : U64, U64, Box(((NodeValue, NodeValue) -> NodeValue)) => U64

	## Create typed scalar signal map nodes
	create_signal_map_i64_i64! : U64, Box((I64 -> I64)) => U64

	create_signal_map_i64_str! : U64, Box((I64 -> Str)) => U64

	create_signal_map2_i64_i64! : U64, U64, Box(((I64, I64) -> I64)) => U64

	create_signal_map2_i64_i64_str! : U64, U64, Box(((I64, I64) -> Str)) => U64

	## Bind a signal to an element's text content
	bind_text! : U64, U64 => {}

	## Bind signals to form element state
	bind_value! : U64, U64 => {}

	bind_checked! : U64, U64 => {}

	bind_disabled! : U64, U64 => {}

	## Bind click events from element to event node
	bind_click! : U64, U64 => {}

	## Bind form input events from element to event node
	bind_input! : U64, U64 => {}

	bind_check! : U64, U64 => {}

	## Fire an event source from Roc code
	send_event! : U64, NodeValue => {}

	## Update a mutable signal from an event carrying the next signal value
	bind_signal_update! : U64, U64 => {}
}
