import Node
import NodeValue exposing [NodeValue]

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
	Element({ tag : Str, attrs : List(Node.Attr), children : List(Elem) }),
	Text(Str),
	TextSignal(Box(Node.SignalExpr)),
	State({ binder : Node.BinderRef, initial : NodeValue, eq : Box((NodeValue, NodeValue -> Bool)), child : Box(Elem) }),
	When({ condition : Box(Node.SignalExpr), when_true : Box(Elem), when_false : Box(Elem) }),
	Each(
		{
			items : Box(Node.SignalExpr),
			key_of : Box((NodeValue -> NodeValue)),
			key_eq : Box((NodeValue, NodeValue -> Bool)),
			row : Box((NodeValue, NodeValue -> Elem)),
		},
	),
]
