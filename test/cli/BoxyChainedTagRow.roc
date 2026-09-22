BoxyChainedTagRow :: [].{}

State := [Input(Str)]

# The body's `[B, ..r]` row is later solved to the annotation's whole row, so
# the checked result row is split across chained tag-union nodes.
make : Str -> Try([A({ items : List(a), rest : State }), B({ name : Str, rest : State }), C(State)], [Bad])
make = |s| Ok(B({ name: s, rest: State.Input(s) }))

use : Str -> Try([A({ items : List(Str), rest : State }), B({ name : Str, rest : State }), C(State)], [Bad])
use = |s| make(s)

expect
	match use("x") {
		Ok(B({ name, rest: _ })) => name == "x"
		_ => False
	}
