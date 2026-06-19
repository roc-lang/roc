import NodeValue exposing [NodeValue]

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
	## Reference to a state/source binder, resolved by the host walk to a dense
	## node id. `BinderRef(local)` names the `local`-th state binder introduced in
	## the current scope (construction order). This is scope-relative: the same
	## `local` in two different scopes refers to two different binders.
	BinderRef := [BinderRef(U64)]

	## A reducer message: applies `transform` to the bound source's current value.
	## The host routes a fired event to the referenced binder and applies the
	## transform. `payload_kind` tells the host the event payload shape so it can
	## marshal the typed payload before calling the transform.
	Msg : {
		binder : BinderRef,
		payload_kind : U64,
		transform : Box((NodeValue, NodeValue -> NodeValue)),
	}

	## Signal expression. `Ref` reads a binder's current value. `ConstValue` is a
	## literal. `Map`/`Map2`/`Combine` are derived nodes carrying boxed typed
	## transforms (confined erasure) and a boxed `is_eq` thunk for change pruning.
	SignalExpr := [
		Ref(BinderRef),
		ConstValue(NodeValue),
		Map({ input : Box(SignalExpr), transform : Box((NodeValue -> NodeValue)) }),
		Map2({ left : Box(SignalExpr), right : Box(SignalExpr), transform : Box((NodeValue, NodeValue -> NodeValue)) }),
		Combine(List(SignalExpr)),
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

	unit_payload_kind : U64
	unit_payload_kind = 1

	str_payload_kind : U64
	str_payload_kind = 2

	bool_payload_kind : U64
	bool_payload_kind = 3

	## Static attribute on a markup element. Dynamic (signal-backed) attrs carry a
	## `SignalExpr`; event handlers carry a `Msg`.
	Attr := [
		StaticText({ field : U64, value : Str }),
		SignalText({ field : U64, signal : Box(SignalExpr) }),
		StaticBool({ field : U64, value : Bool }),
		SignalBool({ field : U64, signal : Box(SignalExpr) }),
		OnEvent({ kind : U64, msg : Msg }),
	]

	## UI element descriptor tree. Markup nodes (`Element`, `Text`, `TextSignal`)
	## carry no identity. Scope/binder nodes are the identity-bearing positions the
	## host walk accounts for:
	## - `State`: introduces a state binder (init value + boxed is_eq thunk) and a
	##   child subtree built with that binder in scope. Advances the scope ordinal.
	## - `When`: a conditional with two arm subtrees; the live arm is its own scope
	##   (`Branch` step). Advances the scope ordinal (its site ordinal).
	## - `Each`: a keyed list; each row is its own scope keyed by the typed key
	##   payload, with a boxed key `is_eq` thunk. The row thunk receives the erased
	##   key and item value. Advances the scope ordinal.
	Elem := [
		Element({ tag : Str, attrs : List(Attr), children : List(Elem) }),
		Text(Str),
		TextSignal(Box(SignalExpr)),
		State({ initial : NodeValue, eq : Box((NodeValue, NodeValue -> Bool)), child : Box(Elem) }),
		When({ condition : Box(SignalExpr), when_true : Box(Elem), when_false : Box(Elem) }),
		Each(
			{
				items : Box(SignalExpr),
				key_of : Box((NodeValue -> NodeValue)),
				key_eq : Box((NodeValue, NodeValue -> Bool)),
				row : Box((NodeValue, NodeValue -> Elem)),
			},
		),
	]
}
