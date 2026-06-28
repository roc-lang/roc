import HostValue exposing [HostValue]

## Pure UI descriptor tree produced by `build`. This is the explicit data the
## host ingests. Identity is NOT threaded in Roc: the tree is immutable and pure,
## and the host assigns construction-order identity by a deterministic pre-order
## walk. Only identity-bearing nodes (state binders, `when` sites, `each` sites)
## advance the per-scope ordinal in that walk; ordinary markup does not.
##
## A `Signal` is an expression that references state/source binders by a binder
## ref (a path-relative index assigned during the host walk). Declaration of a
## binder (via `Ui.state`) mints identity; a use (`map`, sink) does not.
	Node := [].{

		new_token : {} -> Box(U64)
		new_token = |_| Box.box(0)

		## Reference to a state/source binder. The token is minted by `Ui.state` and
	## copied into both the state declaration and all signal/message references to
	## that declaration. The host maps tokens to construction-order node ids during
	## the active descriptor walk; the token is not the state identity.
	BinderRef := [BinderRef(Box(U64))]

	## A reducer message: applies `transform` to the bound source's current value.
	## The host routes a fired event to the referenced binder and applies the
	## transform. `payload_kind` tells the host the event payload shape so it can
	## marshal the typed payload before calling the transform.
	Msg : {
		binder : BinderRef,
		payload_accessor : U64,
		payload_kind : U64,
		payload_reducer : HostValue.EventReducerHandle,
	}

	## Signal expression. `Ref` reads a binder's current value. Other variants
	## carry a copied token allocated at the typed signal construction site, so the
	## host can identify shared derived nodes from explicit data. `ConstValue`
	## carries a boxed value initializer plus output equality. `Map`/`Map2`/
	## `Combine` are derived nodes carrying boxed typed transforms (confined
	## erasure) and a boxed `is_eq` thunk for change pruning. `TaskSource` and
	## `IntervalSource` are host-owned effect sources whose results enter the
	## same signal graph.
	TaskSource : {
		token : Box(U64),
		name : Str,
		cap : HostValue.CapabilityHandle,
		payload_cap : HostValue.CapabilityHandle,
		initial : Box(({} -> HostValue)),
		done : Box((HostValue -> HostValue)),
		failed : Box((HostValue -> HostValue)),
		reset_on_start : Bool,
	}

	IntervalSource : {
		token : Box(U64),
		period_ms : U64,
		cap : HostValue.CapabilityHandle,
		initial : Box(({} -> HostValue)),
		tick : Box((HostValue -> HostValue)),
	}

	SignalExpr := [
		Ref(BinderRef),
		ConstValue(Box(U64), Box(({} -> HostValue)), HostValue.CapabilityHandle),
		Map(Box(U64), Box(SignalExpr), Box((HostValue -> HostValue)), HostValue.CapabilityHandle),
		Map2(Box(U64), Box(SignalExpr), Box(SignalExpr), Box((HostValue, HostValue -> HostValue)), HostValue.CapabilityHandle),
		Combine(Box(U64), List(SignalExpr), Box((List(HostValue) -> HostValue)), HostValue.CapabilityHandle),
		TaskSource(TaskSource),
		IntervalSource(IntervalSource),
	]

	Cmd := [
		StartTask(
			{
				task_token : Box(U64),
				task_name : Str,
				request_init : Box(({} -> HostValue)),
				request_read : HostValue.TaskRequestReadHandle,
			},
		),
	]

	Cleanup := [
		Cleanup(Str),
	]

	## Text/attr sink fields, matching the host's render field discriminants.
	field_text : U64
	field_text = 1

	field_role : U64
	field_role = 2

	field_label : U64
	field_label = 3

	field_test_id : U64
	field_test_id = 4

	field_value : U64
	field_value = 5

	field_class : U64
	field_class = 6

	field_custom : U64
	field_custom = 7

	bool_field_checked : U64
	bool_field_checked = 1

	bool_field_disabled : U64
	bool_field_disabled = 2

	event_kind_click : U64
	event_kind_click = 1

	event_kind_input : U64
	event_kind_input = 2

	event_kind_check : U64
	event_kind_check = 3

	event_kind_pointer_down : U64
	event_kind_pointer_down = 4

	event_kind_pointer_up : U64
	event_kind_pointer_up = 5

	event_kind_pointer_enter : U64
	event_kind_pointer_enter = 6

	event_kind_pointer_leave : U64
	event_kind_pointer_leave = 7

	unit_payload_kind : U64
	unit_payload_kind = 1

	str_payload_kind : U64
	str_payload_kind = 2

	bool_payload_kind : U64
	bool_payload_kind = 3

	bytes_payload_kind : U64
	bytes_payload_kind = 4

	payload_accessor_none : U64
	payload_accessor_none = 1

	payload_accessor_target_value : U64
	payload_accessor_target_value = 2

	payload_accessor_target_checked : U64
	payload_accessor_target_checked = 3

	payload_accessor_record_key_shift : U64
	payload_accessor_record_key_shift = 4

	listener_prevent_default : U64
	listener_prevent_default = 1

	listener_stop_propagation : U64
	listener_stop_propagation = 2

	listener_capture : U64
	listener_capture = 4

	listener_passive : U64
	listener_passive = 8

	listener_once : U64
	listener_once = 16

	## Static attribute on a markup element. Dynamic (signal-backed) attrs carry a
	## `SignalExpr`; event handlers carry a `Msg`.
	Attr := [
		StaticText({ field : U64, name : Str, value : Str }),
		SignalText({ field : U64, name : Str, signal : Box(SignalExpr), read : HostValue.TextReadHandle }),
		StaticBool({ field : U64, value : Bool }),
		SignalBool({ field : U64, signal : Box(SignalExpr), read : HostValue.BoolReadHandle }),
		OnEvent({ kind : U64, msg : Msg }),
		OnNamedEvent({ name : Str, options : U64, msg : Msg }),
	]
}
