import Node
import HostValue exposing [HostValue]

## UI element descriptor tree. Markup nodes (`Element`, `Text`, `TextSignal`)
## carry no identity. Scope/binder nodes are the identity-bearing positions the
## host walk accounts for:
## - `State`: introduces a state binder (boxed init thunk + boxed is_eq thunk)
##   and a child subtree built with that binder in scope. Advances the scope
##   ordinal.
## - `When`: a conditional with two arm subtrees; the live arm is its own scope
##   (`Branch` step). Advances the scope ordinal (its site ordinal).
## - `Each`: a keyed list; each row is its own scope keyed by the typed key
##   payload, with a boxed key `is_eq` thunk. The row thunk receives the host-owned
##   key and item value. Advances the scope ordinal.
Elem := [
	Element({ tag : Str, attrs : List(Node.Attr), children : List(Elem) }),
	Text(Str),
	TextSignal({ signal : Box(Node.SignalExpr), read : Box((HostValue -> Str)) }),
	State({ binder : Node.BinderRef, initial : Box(({} -> HostValue)), eq : Box((HostValue, HostValue -> Bool)), drop : Box((HostValue -> {})), child : Box(Elem) }),
	When({ condition : Box(Node.SignalExpr), read : Box((HostValue -> Bool)), when_true : Box(Elem), when_false : Box(Elem) }),
	Each(
		{
			items : Box(Node.SignalExpr),
			items_to_values : Box((HostValue -> List(HostValue))),
			key_of : Box((HostValue -> HostValue)),
			key_eq : Box((HostValue, HostValue -> Bool)),
			key_drop : Box((HostValue -> {})),
			item_eq : Box((HostValue, HostValue -> Bool)),
			item_drop : Box((HostValue -> {})),
			row : Box((HostValue, HostValue -> Elem)),
		},
	),
]
